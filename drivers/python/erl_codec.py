"""
Implementation of the Erlang binary protocol.

Provides facilities to work with Standard I/O streams, sockets, and
Erlang binary messages.
"""

import socket
from binascii import hexlify
from struct import pack, unpack
from collections import deque
from types import NoneType
from cStringIO import StringIO
from sys import stdin, stdout


class StdioWrapper(object):
    """
    Wraps Standard I/O input and output facilities; exposes a standard
    stream interface.
    """

    def read(self, num):
        """Read the specified number of bytes."""
        return stdin.read(num)

    def write(self, data):
        """Write the given data."""
        return stdout.write(data)

    def flush(self):
        """Override buffering and flush all data to Standard Out."""
        stdout.flush()

    def close(self):
        """Exist for interface completeness."""
        pass


class SocketWrapper(object):
    """
    Wraps socket creation and usage logic in the standard stream interface.
    """

    def __init__(self, host, port):
        """Open a socket to the given host and port."""
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))

    def read(self, num):
        """Read the specified number of bytes."""
        chunks = []
        len_read_so_far = 0
        while len_read_so_far < num:
            chunk = self.socket.recv(num - len_read_so_far)
            len_read_so_far += len(chunk)
            chunks.append(chunk)
        return ''.join(chunks)

    def write(self, data):
        """Write the given data."""
        return self.socket.sendall(data)

    def flush(self):
        """Exist for interface completeness."""
        pass

    def close(self):
        """Close the socket."""
        self.socket.close()


class AtomCacheRef(object):
    """Implements an Erlang atom cache ref."""

    def __init__(self, value):
        """Set the given value as the value of this cache ref."""
        self.value = value

    def __str__(self):
        """Return the human-readble representation."""
        return 'AtomCacheRef: %s' % self.value

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'AtomCacheRef(%s)' % repr(self.value)

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.value == other.value


class Atom(object):
    """Implements an Erlang atom."""

    def __init__(self, name):
        """Set the given name on the atom."""
        self.name = name

    def __str__(self):
        """Return the human-readble representation."""
        return self.name

    def __repr__(self):
        """Return the Pythonic representation."""
        return "Atom(%s)" % repr(self.name)

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.name == other.name


class Reference(object):
    """Implements an Erlang reference."""

    def __init__(self, atom, identifier, creation):
        """Set the given data on the object."""
        self.atom = atom
        self.identifier = identifier
        self.creation = creation

    def __str__(self):
        """Return the human-readble representation."""
        return '%s#Ref<%s, %s>' % (
            self.atom, self.creation, self.identifier)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Reference(%s, %s, %s)' % (
            repr(self.atom), repr(self.identifier), repr(self.creation))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.atom == other.atom and \
               self.identifier == other.identifier and \
               self.creation == other.creation


class NewReference(object):
    """
    Implements an Erlang "new reference" (a reference created at runtime).
    """

    def __init__(self, atom, creation, ids):
        """Set the given data on the object."""
        self.atom = atom
        self.creation = creation
        self.ids = ids

    def __str__(self):
        """Return the human-readble representation."""
        return '%s#Ref<%s.%s>' % (
            self.atom, self.creation, '.'.join(str(x) for x in self.ids))

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'NewReference(%s, %s, %s)' % (
            repr(self.atom),
            repr(self.creation),
            repr(self.ids))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.atom == other.atom and \
               self.creation == other.creation and \
               self.ids == other.ids


class Port(object):
    """Implements an Erlang port."""

    def __init__(self, atom, identifier, creation):
        """Set the given data on the object."""
        self.atom = atom
        self.identifier = identifier
        self.creation = creation

    def __str__(self):
        """Return the human-readble representation."""
        return '%s#Port<%s.%s>' % (
            self.atom, self.creation, self.identifier)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Port(%s, %s, %s)' % (
            repr(self.atom), repr(self.identifier), repr(self.creation))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.atom == other.atom and \
               self.identifier == other.identifier and \
               self.creation == other.creation


class Pid(object):
    """Implements an Erlang pid (process identifier)."""

    def __init__(self, atom, identifier, serial, creation):
        """Set the given data on the object."""
        self.atom = atom
        self.identifier = identifier
        self.serial = serial
        self.creation = creation

    def __str__(self):
        """Return the human-readble representation."""
        return '%s:<%s.%s.%s>' % (
            self.atom, self.serial, self.identifier, self.creation)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Pid(%s, %s, %s, %s)' % (
            repr(self.atom),
            repr(self.identifier),
            repr(self.serial),
            repr(self.creation))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.atom == other.atom and \
               self.identifier == other.identifier and \
               self.serial == other.serial and \
               self.creation == other.creation


class Binary(object):
    """
    Implements an Erlang binary data container.

    This is the fastest way to transfer data around.
    """

    def __init__(self, data):
        """Set the given data on the binary object."""
        self.data = data

    def __str__(self):
        """Return the human-readble representation."""
        return '<<%s>>' % ','.join(str(ord(x)) for x in self.data)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Binary(%s)' % repr(self.data)

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.data == other.data


class BitBinary(object):
    """
    Implements an Erlang bit binary (a more efficient way of encoding a
    string).
    """

    def __init__(self, bits, data):
        """Set the given data on the object."""
        self.bits = bits
        self.data = data

    def __str__(self):
        """Return the human-readble representation."""
        init = ','.join(str(ord(x)) for x in self.data[0:-1])
        return '<<%s, %s:%s>>' % (init, ord(self.data[-1]), self.bits)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'BitBinary(%s, %s)' % (repr(self.bits), repr(self.data))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.bits == other.bits and \
               self.data == other.data


class Export(object):
    """Implements an Erlang export."""

    def __init__(self, module, function, arity):
        """Set the given data on the object."""
        self.module = module
        self.function = function
        self.arity = arity

    def __str__(self):
        """Return the human-readble representation."""
        return '%s:%s/%s' % (
            self.module, self.function, self.arity)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Export(%s, %s, %s)' % (
            repr(self.module), repr(self.function), repr(self.arity))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.module == other.module and \
               self.function == other.function and \
               self.arity == other.arity


class Function(object):
    """Implements an Erlang function (defined at compile-time)."""

    def __init__(self, pid, module, index, uniq, free_vars):
        """Set the given data on the object."""
        self.pid = pid
        self.module = module
        self.index = index
        self.uniq = uniq
        self.free_vars = free_vars

    def __str__(self):
        """Return the human-readble representation."""
        return '%s:%s' % (self.module, self.uniq)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Function(%s, %s, %s, %s, %s)' % (
            repr(self.pid),
            repr(self.module),
            repr(self.index),
            repr(self.uniq),
            repr(self.free_vars))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.pid == other.pid and \
               self.module == other.module and \
               self.index == other.index and \
               self.uniq == other.uniq and \
               self.free_vars == other.free_vars


class NewFunction(object):
    """
    Implements an Erlang function (created at run-time, usually with
    the fun () -> end syntax).
    """

    def __init__(
      self, arity, uniq, index, module, old_index,
      old_uniq, pid, free_vars):
        """Set the given data on the object."""
        self.arity = arity
        self.uniq = uniq
        self.index = index
        self.module = module
        self.old_index = old_index
        self.old_uniq = old_uniq
        self.pid = pid
        self.free_vars = free_vars

    def hexuniq(self):
        """Return a HEX representation of the uniq property."""
        return hexlify(self.uniq)

    def __str__(self):
        """Return the human-readble representation."""
        return '%s:%s/%s' % (self.module, self.hexuniq(), self.arity)

    def __repr__(self):
        """Return the Pythonic representation."""
        return "NewFunction(%s, %s, %s, %s, %s, %s, %s, [%s])" % (
            repr(self.arity), repr(self.uniq), repr(self.index),
            repr(self.module), repr(self.old_index), repr(self.old_uniq),
            repr(self.pid), ','.join(repr(x) for x in self.free_vars))

    def __eq__(self, other):
        """Compare self with another object of the same type."""
        return self.arity == other.arity and \
               self.uniq == other.uniq and \
               self.index == other.index and \
               self.module == other.module and \
               self.old_index == other.old_index and \
               self.old_uniq == other.old_uniq and \
               self.pid == other.pid and \
               self.free_vars == other.free_vars


def proplist_to_dict(proplist):
    """Turn an Erlang-style proplist into a dict."""
    result = {}
    for element in proplist:
        if len(element) < 2:
            raise ValueError(
                'Proplist elements should have at least 2 elements')
        result[element[0]] = element[1]
    return result


def decode_atom_ext(stream):
    """Decode and return an Erlang atom."""
    atom_len, = unpack('>h', stream.read(2))
    return Atom(stream.read(atom_len))


def decode_reference_ext(stream):
    """Decode and return an Erlang reference."""
    atom = decode(stream, False)
    identifier, = unpack('>L', stream.read(4))
    creation = ord(stream.read(1))
    return Reference(atom, identifier, creation)


def decode_port_ext(stream):
    """Decode and return an Erlang port."""
    atom = decode(stream, False)
    identifier, = unpack('>L', stream.read(4))
    creation = ord(stream.read(1))
    return Port(atom, identifier, creation)


def decode_pid_ext(stream):
    """Decode and return an Erlang pid."""
    atom = decode(stream, False)
    identifier, = unpack('>L', stream.read(4))
    serial, = unpack('>L', stream.read(4))
    creation = ord(stream.read(1))
    return Pid(atom, identifier, serial, creation)


def decode_small_tuple_ext(stream):
    """Decode and return a small Erlang tuple (fewer than 256 elements)."""
    tuple_len = ord(stream.read(1))
    elements = []
    for _index in range(tuple_len):
        value = decode(stream, False)
        elements.append(value)
    return tuple(elements)


def decode_large_tuple_ext(stream):
    """Decode and return a large Erlang tuple (more than 256 elements)."""
    tuple_len, = unpack('>L', stream.read(4))
    elements = []
    for _index in range(tuple_len):
        value = decode(stream, False)
        elements.append(value)
    return tuple(elements)


def decode_nil_ext(_stream):
    """Decode and return a nil/null/None."""
    return None


def decode_string_ext(stream):
    """Decode and return a string."""
    str_len, = unpack('>h', stream.read(2))
    return stream.read(str_len)


def decode_list_ext(stream):
    """
    Decode and return a list.

    Depending on the list contents, a string may be returned. This will
    be the case if the list contains only byte values, which means that
    the list is actually intending to be a string, but being capped by
    Erlang's 65K char limit for strings (before they overflow into a list).
    """
    list_len, = unpack('>L', stream.read(4))
    elements = []
    is_str = True
    for _index in range(list_len):
        value = decode(stream, False)
        is_str = is_str and type(value) == int and value < 256
        elements.append(value)
    tail = decode(stream, False)
    if tail is not None:
        is_str = is_str and type(tail) == int and tail < 256
        elements.append(tail)
    if is_str:
        return pack('B' * len(elements), *elements)
    else:
        return elements


def decode_binary_ext(stream):
    """Decode and return an Erlang binary."""
    bin_len, = unpack('>L', stream.read(4))
    return Binary(stream.read(bin_len))


def decode_small_big_ext(stream):
    """Decode and return "small" big number."""
    num_bytes = ord(stream.read(1))
    sign = ord(stream.read(1))
    num = 0
    for i in range(num_bytes):
        num += ord(stream.read(1)) * 256 ** i
    if sign == 1:
        num *= -1
    return num


def decode_large_big_ext(stream):
    """Decode and return "large" big number."""
    num_bytes, = unpack('>L', stream.read(4))
    sign = ord(stream.read(1))
    num = 0
    for i in range(num_bytes):
        num += ord(stream.read(1)) * 256 ** i
    if sign == 1:
        num *= -1
    return num


def decode_new_reference_ext(stream):
    """Decode and return an Erlang "new reference"."""
    length, = unpack('>h', stream.read(2))
    atom = decode(stream, False)
    creation = ord(stream.read(1))
    identifiers = deque()
    for _index in range(length):
        identifier, = unpack('>L', stream.read(4))
        identifiers.appendleft(identifier)
    return NewReference(atom, creation, list(identifiers))


def decode_small_atom_ext(stream):
    """Decode and return a small Erlang atom."""
    atom_len = ord(stream.read(1))
    atom_name = stream.read(atom_len)
    return Atom(atom_name)


def decode_fun_ext(stream):
    """Decode and return an Erlang function."""
    num_free, = unpack('>L', stream.read(4))
    pid = decode(stream, False)
    module = decode(stream, False)
    index = decode(stream, False)
    uniq = decode(stream, False)
    free_vars = []
    for _index in range(num_free):
        free_var = decode(stream, False)
        free_vars.append(free_var)
    if not len(free_vars):
        free_vars = None

    return Function(pid, module, index, uniq, free_vars)


def decode_new_fun_ext(stream):
    """Decode and return an Erlang "new function"."""
    _size, = unpack('>L', stream.read(4))
    arity = ord(stream.read(1))
    uniq = stream.read(16)
    index, = unpack('>L', stream.read(4))
    num_free, = unpack('>L', stream.read(4))
    module = decode(stream, False)
    old_index = decode(stream, False)
    old_uniq = decode(stream, False)
    pid = decode(stream, False)
    free_vars = []
    for _index in range(num_free):
        free_var = decode(stream, False)
        free_vars.append(free_var)
    if not len(free_vars):
        free_vars = None

    return NewFunction(
        arity, uniq, index, module, old_index, old_uniq, pid, free_vars)


def decode_export_ext(stream):
    """Decode and return an Erlang export."""
    module = decode(stream, False)
    function = decode(stream, False)
    arity = decode(stream, False)
    return Export(module, function, arity)


def decode_new_float_ext(stream):
    """Decode and return an IEEE 8-byte floating-point number."""
    return unpack('>d', stream.read(8))[0]


def decode_bit_binary_ext(stream):
    """Decode and return an Erlang bit binary."""
    length = unpack('>L', stream.read(4))[0]
    return BitBinary(ord(stream.read(1)), stream.read(length))


def decode_atom_cache_ref(stream):
    """Decode and return an Erlang atom cache ref."""
    return AtomCacheRef(ord(stream.read(1)))


def decode_small_integer_ext(stream):
    """Decode and return a small integer (byte)."""
    return ord(stream.read(1))


def decode_integer_ext(stream):
    """Decode and return an integer."""
    return unpack('>l', stream.read(4))[0]


def decode_float_ext(stream):
    """Decode and return a float (represented by Erlang as a string)."""
    return float(''.join(x for x in stream.read(31) if ord(x) > 0))


def decode(stream, check_dist_tag=True):
    """
    Decode a single value from the given stream and return it.

    If check_dist_tag, check to see that the first byte is 131 (this is
    how Erlang flags the beginning of every data type). This check does
    not need to be performed when recursively decoding nested data types,
    hence the optional argument.
    """
    first_byte = ord(stream.read(1))
    if check_dist_tag:
        if first_byte != 131:
            raise ValueError('this is not an Erlang EXT datatype')
        else:
            ext_code = ord(stream.read(1))
    else:
        ext_code = first_byte

    if ext_code == 70:
        return decode_new_float_ext(stream)
    elif ext_code == 77:
        return decode_bit_binary_ext(stream)
    elif ext_code == 82:
        return decode_atom_cache_ref(stream)
    elif ext_code == 97:
        return decode_small_integer_ext(stream)
    elif ext_code == 98:
        return decode_integer_ext(stream)
    elif ext_code == 99:
        return decode_float_ext(stream)
    elif ext_code == 100:
        return decode_atom_ext(stream)
    elif ext_code == 101:
        return decode_reference_ext(stream)
    elif ext_code == 102:
        return decode_port_ext(stream)
    elif ext_code == 103:
        return decode_pid_ext(stream)
    elif ext_code == 104:
        return decode_small_tuple_ext(stream)
    elif ext_code == 105:
        return decode_large_tuple_ext(stream)
    elif ext_code == 106:
        return decode_nil_ext(stream)
    elif ext_code == 107:
        return decode_string_ext(stream)
    elif ext_code == 108:
        return decode_list_ext(stream)
    elif ext_code == 109:
        return decode_binary_ext(stream)
    elif ext_code == 110:
        return decode_small_big_ext(stream)
    elif ext_code == 111:
        return decode_large_big_ext(stream)
    elif ext_code == 112:
        return decode_new_fun_ext(stream)
    elif ext_code == 113:
        return decode_export_ext(stream)
    elif ext_code == 114:
        return decode_new_reference_ext(stream)
    elif ext_code == 115:
        return decode_small_atom_ext(stream)
    elif ext_code == 117:
        return decode_fun_ext(stream)
    else:
        raise ValueError(
            'Unable to decode Erlang EXT data type: %s' % ext_code)


def encode_float(data, stream):
    """Encode a floating-point number into the stream."""
    stream.write(chr(70))
    stream.write(pack('>d', data))


def encode_bit_binary(data, stream):
    """Encode an Erlang bit binary into the stream."""
    stream.write(chr(77))
    stream.write(pack('>L', len(data.data)))
    stream.write(chr(data.bits))
    stream.write(data.data)


def encode_atom_cache_ref(data, stream):
    """Encode an Erlang atom cache ref into the stream."""
    stream.write(chr(82))
    stream.write(chr(data.value))


def encode_small_integer(data, stream):
    """Encode a small integer (byte) into the stream."""
    stream.write(chr(97))
    stream.write(chr(data))


def encode_integer(data, stream):
    """Encode an integer into the stream."""
    stream.write(chr(98))
    stream.write(pack('>l', data))


def encode_long(data, stream):
    """Encode a large number into the stream."""
    if data < 0:
        data *= -1
        sign = 1
    else:
        sign = 0
    byte_list = []

    while data != 0:
        byte_list.append(data & 255)
        data = data >> 8
    byte_list_len = len(byte_list)
    if byte_list_len <= 255:
        stream.write(chr(110))
        stream.write(chr(byte_list_len))
    else:
        stream.write(chr(111))
        stream.write(pack('>L', len(byte_list)))
    stream.write(chr(sign))
    stream.write(''.join(chr(x) for x in byte_list))


def encode_number(data, stream):
    """Encode any-size number into the stream."""
    if 0 <= data <= 0xff:
        encode_small_integer(data, stream)
    elif -0x7fffffff - 1 <= data <= 0x7fffffff:
        encode_integer(data, stream)
    else:
        encode_long(data, stream)


def encode_atom(data, stream):
    """Encode an Erlang atom into the stream."""
    name_len = len(data.name)
    if name_len <= 0xf:
        stream.write(chr(115))
        stream.write(chr(name_len))
    else:
        stream.write(chr(100))
        stream.write(pack('>h', name_len))
    stream.write(data.name)


def encode_reference(data, stream):
    """Encode an Erlang reference into the stream."""
    stream.write(chr(101))
    encode(data.atom, stream, False)
    stream.write(pack('>L', data.identifier))
    stream.write(chr(data.creation))


def encode_port(data, stream):
    """Encode an Erlang port into the stream."""
    stream.write(chr(102))
    encode(data.atom, stream, False)
    stream.write(pack('>L', data.identifier))
    stream.write(chr(data.creation))


def encode_pid(data, stream):
    """Encode an Erlang pid into the stream."""
    stream.write(chr(103))
    encode(data.atom, stream, False)
    stream.write(pack('>L', data.identifier))
    stream.write(pack('>L', data.serial))
    stream.write(chr(data.creation))


def encode_tuple(data, stream):
    """Encode a tuple into the stream."""
    data_len = len(data)
    if data_len < 256:
        stream.write(chr(104))
        stream.write(chr(data_len))
    else:
        stream.write(chr(105))
        stream.write(pack('>L', data_len))
    for i in range(data_len):
        encode(data[i], stream, False)


def encode_none(_data, stream):
    """Encode a NoneType into the stream (as Erlang nil)."""
    stream.write(chr(106))


def encode_str(data, stream):
    """Encode a string into the stream."""
    data_len = len(data)
    if data_len > 0xffff:
        encode_list(data, stream)
    else:
        stream.write(chr(107))
        stream.write(pack('>h', data_len))
        stream.write(data)


def encode_list(data, stream):
    """Encode a list into the stream."""
    data_len = len(data)
    stream.write(chr(108))
    stream.write(pack('>L', data_len))
    for i in range(data_len):
        encode(data[i], stream, False)
    stream.write(chr(106))


def encode_binary(data, stream):
    """Encode an Erlang binary into the stream."""
    stream.write(chr(109))
    stream.write(pack('>L', len(data.data)))
    stream.write(data.data)


def encode_new_reference(data, stream):
    """Encode an Erlang new reference into the stream."""
    stream.write(chr(114))
    ids_len = len(data.ids)
    stream.write(pack('>h', ids_len))
    encode(data.atom, stream, False)
    stream.write(chr(data.creation))
    for identifier in data.ids:
        stream.write(pack('>L', identifier))


def encode_function(data, stream):
    """Encode an Erlang function into the stream."""
    stream.write(chr(117))
    if data.free_vars is None:
        free_vars_len = 0
    else:
        free_vars_len = len(data.free_vars)
    stream.write(pack('>L', free_vars_len))
    encode(data.pid, stream, False)
    encode(data.module, stream, False)
    encode(data.index, stream, False)
    encode(data.uniq, stream, False)
    if free_vars_len > 0:
        for free_var in data.free_vars:
            stream.write(pack('>L', free_var))


def encode_new_function(data, stream):
    """Encode an Erlang "new function" into the stream."""
    stream.write(chr(112))
    if data.free_vars is None:
        free_vars_len = 0
    else:
        free_vars_len = len(data.free_vars)

    byte_list = StringIO()
    byte_list.write(chr(data.arity))
    byte_list.write(data.uniq)
    byte_list.write(pack('>L', data.index))
    byte_list.write(pack('>L', free_vars_len))
    encode(data.module, byte_list, False)
    encode(data.old_index, byte_list, False)
    encode(data.old_uniq, byte_list, False)
    encode(data.pid, byte_list, False)
    if free_vars_len > 0:
        for free_var in data.free_vars:
            byte_list.write(pack('>L', free_var))
    byte_list_value = byte_list.getvalue()
    byte_list.close()
    stream.write(pack('>L', len(byte_list_value) + 4))
    stream.write(byte_list_value)


def encode_export(data, stream):
    """Encode an Erlang export into the stream."""
    stream.write(chr(113))
    encode(data.module, stream, False)
    encode(data.function, stream, False)
    encode(data.arity, stream, False)


def encode_dict(data, stream):
    """Encode a dict into the stream (as a property list)."""
    encode(zip(data.keys(), data.values()), stream, False)


def encode(data, stream, send_magic_byte=True):
    """
    Encode the given data into the given stream.

    If send_magic_byte, the value 131 is sent before anything (this is
    how Erlang denotes that there is a new piece of data coming across).
    However, for nested data, this only needs to be sent once, hence
    the optional argument.
    """
    if send_magic_byte:
        stream.write(chr(131))

    data_type = type(data)
    if data_type == float:
        encode_float(data, stream)
    elif data_type == BitBinary:
        encode_bit_binary(data, stream)
    elif data_type == AtomCacheRef:
        encode_atom_cache_ref(data, stream)
    elif data_type == int:
        encode_number(data, stream)
    elif data_type == long:
        encode_number(data, stream)
    elif data_type == Atom:
        encode_atom(data, stream)
    elif data_type == Reference:
        encode_reference(data, stream)
    elif data_type == Port:
        encode_port(data, stream)
    elif data_type == Pid:
        encode_pid(data, stream)
    elif data_type == tuple:
        encode_tuple(data, stream)
    elif data_type == NoneType:
        encode_none(data, stream)
    elif data_type == str:
        encode_str(data, stream)
    elif data_type == list:
        encode_list(data, stream)
    elif data_type == Binary:
        encode_binary(data, stream)
    elif data_type == NewReference:
        encode_new_reference(data, stream)
    elif data_type == Function:
        encode_function(data, stream)
    elif data_type == NewFunction:
        encode_new_function(data, stream)
    elif data_type == Export:
        encode_export(data, stream)
    elif data_type == dict:
        encode_dict(data, stream)
    else:
        encode(data.to_erlang(), stream, False)


class Gateway(object):
    """
    Implements a class that can be used to conveniently interface with
    Hurricane to send/receive messages.
    """

    stream = None

    def __init__(self, stream=None):
        """
        Initialize with an optional stream. If no stream is provided,
        Standard I/O will be used.
        """
        if stream is None:
            self.set_stream(StdioWrapper())
        else:
            self.set_stream(stream)

    def set_stream(self, stream):
        """Close any open stream and set the new one."""
        self.close()
        self.stream = stream

    def close(self):
        """If there is an active stream, close it."""
        if self.stream:
            self.stream.close()

    def recv(self):
        """Receive one message from Hurricane."""
        message_len = self.stream.read(4)

        if len(message_len) < 4:
            raise ValueError('Message size payload should be 4 bytes')

        message_len, = unpack('>L', message_len)
        stream_wrapper = StringIO()
        stream_wrapper.write(self.stream.read(message_len))
        stream_wrapper.seek(0)
        message = decode(stream_wrapper)
        stream_wrapper.close()
        return message

    def send(self, message):
        """Send one message to Hurricane."""
        stream_wrapper = StringIO()
        encode(message, stream_wrapper)
        self.stream.write(
            pack('>L', len(stream_wrapper.getvalue())))
        self.stream.write(stream_wrapper.getvalue())
        stream_wrapper.close()
        self.stream.flush()
