from binascii import hexlify
from struct import pack, unpack
from collections import deque
from types import NoneType
from cStringIO import StringIO
from sys import stdin, stdout, stderr

class StreamEmulator(object):
    def __init__(self, data=None):
        if data is None:
            self.data = ''
        elif type(data) == list:
            self.data = ''.join([chr(x) for x in data])
        else:
            self.data = data
        self.pos = 0

    def read(self, bytes):
        if len(self.data) < self.pos + bytes:
            raise ValueError(
                'Out of data to read (was asked for %s '
                'byte(s), only %s byte(s) remain)' % \
                (bytes, len(self.data) - self.pos))
        read_data = self.data[self.pos:self.pos + bytes]
        self.pos += bytes
        return read_data

    def write(self, data):
        if type(data) == list:
            self.data += ''.join([chr(x) for x in data])
        else:
            self.data += data

    def flush(self):
        pass

    def clear(self):
        self.data = ''
        self.pos = 0

class AtomCacheRef(object):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return 'AtomCacheRef: %s' % self.value

    def __repr__(self):
        return 'AtomCacheRef(%s)' % repr(self.value)

    def __eq__(self, other):
        return self.value == other.value

class Atom(object):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return "Atom(%s)" % repr(self.name)

    def __eq__(self, other):
        return self.name == other.name

class Reference(object):
    def __init__(self, atom, identifier, creation):
        self.atom = atom
        self.identifier = identifier
        self.creation = creation

    def __str__(self):
        return '%s#Ref<%s, %s>' % (
            self.atom, self.creation, self.identifier)

    def __repr__(self):
        return 'Reference(%s, %s, %s)' % (
            repr(self.atom), repr(self.identifier), repr(self.creation))

    def __eq__(self, other):
        return self.atom == other.atom and \
               self.identifier == other.identifier and \
               self.creation == other.creation

class NewReference(object):
    def __init__(self, atom, creation, ids):
        self.atom = atom
        self.creation = creation
        self.ids = ids

    def __str__(self):
        return '%s#Ref<%s.%s>' % (
            self.atom, self.creation, '.'.join([str(x) for x in self.ids]))
        
    def __repr__(self):
        return 'NewReference(%s, %s, %s)' % (
            repr(self.atom),
            repr(self.creation),
            repr(self.ids))

    def __eq__(self, other):
        return self.atom == other.atom and \
               self.creation == other.creation and \
               self.ids == other.ids

class Port(object):
    def __init__(self, atom, identifier, creation):
        self.atom = atom
        self.identifier = identifier
        self.creation = creation

    def __str__(self):
        return '%s#Port<%s.%s>' % (
            self.atom, self.creation, self.identifier)

    def __repr__(self):
        return 'Port(%s, %s, %s)' % (
            repr(self.atom), repr(self.identifier), repr(self.creation))

    def __eq__(self, other):
        return self.atom == other.atom and \
               self.identifier == other.identifier and \
               self.creation == other.creation

class Pid(object):
    def __init__(self, atom, identifier, serial, creation):
        self.atom = atom
        self.identifier = identifier
        self.serial = serial
        self.creation = creation

    def __str__(self):
        return '%s:<%s.%s.%s>' % (
            self.atom, self.serial, self.identifier, self.creation)

    def __repr__(self):
        return 'Pid(%s, %s, %s, %s)' % (
            repr(self.atom),
            repr(self.identifier),
            repr(self.serial),
            repr(self.creation))

    def __eq__(self, other):
        return self.atom == other.atom and \
               self.identifier == other.identifier and \
               self.serial == other.serial and \
               self.creation == other.creation

class Binary(object):
    def __init__(self, data):
        self.data = data

    def __str__(self):
        return '<<%s>>' % ','.join([str(ord(x)) for x in self.data])

    def __repr__(self):
        return 'Binary(%s)' % repr(self.data)

    def __eq__(self, other):
        return self.data == other.data

class BitBinary(object):
    def __init__(self, bits, data):
        self.bits = bits
        self.data = data

    def __str__(self):
        init = ','.join([str(ord(x)) for x in self.data[0:-1]])
        return '<<%s, %s:%s>>' % (init, ord(self.data[-1]), self.bits)

    def __repr__(self):
        return 'BitBinary(%s, %s)' % (repr(self.bits), repr(self.data))

    def __eq__(self, other):
        return self.bits == other.bits and \
               self.data == other.data

class Export(object):
    def __init__(self, module, function, arity):
        self.module = module
        self.function = function
        self.arity = arity

    def __str__(self):
        return '%s:%s/%s' % (
            self.module, self.function, self.arity)

    def __repr__(self):
        return 'Export(%s, %s, %s)' % (
            repr(self.module), repr(self.function), repr(self.arity))

    def __eq__(self, other):
        return self.module == other.module and \
               self.function == other.function and \
               self.arity == other.arity

class Function(object):
    def __init__(self, pid, module, index, uniq, free_vars):
        self.pid = pid
        self.module = module
        self.index = index
        self.uniq = uniq
        self.free_vars = free_vars

    def __str__(self):
        return '%s:%s' % (self.module, self.uniq)

    def __repr__(self):
        return 'Function(%s, %s, %s, %s, %s)' % (
            repr(self.pid),
            repr(self.module),
            repr(self.index),
            repr(self.uniq),
            repr(self.free_vars))

    def __eq__(self, other):
        return self.pid == other.pid and \
               self.module == other.module and \
               self.index == other.index and \
               self.uniq == other.uniq and \
               self.free_vars == other.free_vars

class NewFunction(object):
    def __init__(
        self, arity, uniq, index, module, old_index,
        old_uniq, pid, free_vars):
        self.arity = arity
        self.uniq = uniq
        self.index = index
        self.module = module
        self.old_index = old_index
        self.old_uniq = old_uniq
        self.pid = pid
        self.free_vars = free_vars

    def hexuniq(self):
        return hexlify(self.uniq)

    def __str__(self):
        return '%s:%s/%s' % (self.module, self.hexuniq(), self.arity)

    def __repr__(self):
        return "NewFunction(%s, %s, %s, %s, %s, %s, %s, [%s])" % (
            repr(self.arity), repr(self.uniq), repr(self.index),
            repr(self.module), repr(self.old_index), repr(self.old_uniq),
            repr(self.pid), ','.join([repr(x) for x in self.free_vars]))

    def __eq__(self, other):
        return self.arity == other.arity and \
               self.uniq == other.uniq and \
               self.index == other.index and \
               self.module == other.module and \
               self.old_index == other.old_index and \
               self.old_uniq == other.old_uniq and \
               self.pid == other.pid and \
               self.free_vars == other.free_vars

def decode_atom_ext(stream):
    atom_len, = unpack('>h', stream.read(2))
    return Atom(stream.read(atom_len))

def decode_reference_ext(stream):
    atom = decode(stream, False)
    identifier, = unpack('>L', stream.read(4))
    creation = ord(stream.read(1))
    return Reference(atom, identifier, creation)

def decode_port_ext(stream):
    atom = decode(stream, False)
    identifier, = unpack('>L', stream.read(4))
    creation = ord(stream.read(1))
    return Port(atom, identifier, creation)

def decode_pid_ext(stream):
    atom = decode(stream, False)
    identifier, = unpack('>L', stream.read(4))
    serial, = unpack('>L', stream.read(4))
    creation = ord(stream.read(1))
    return Pid(atom, identifier, serial, creation)

def decode_small_tuple_ext(stream):
    tuple_len = ord(stream.read(1))
    elements = []
    for i in range(tuple_len):
        value = decode(stream, False)
        elements.append(value)
    return tuple(elements)

def decode_large_tuple_ext(stream):
    tuple_len, = unpack('>L', stream.read(4))
    elements = []
    for i in range(tuple_len):
        value = decode(stream, False)
        elements.append(value)
    return tuple(elements)

def decode_nil_ext(stream):
    return None

def decode_string_ext(stream):
    str_len, = unpack('>h', stream.read(2))
    return stream.read(str_len)

def decode_list_ext(stream):
    list_len, = unpack('>L', stream.read(4))
    elements = []
    is_str = True
    for i in range(list_len):
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
    bin_len, = unpack('>L', stream.read(4))
    return Binary(stream.read(bin_len))

def decode_small_big_ext(stream):
    num_bytes = ord(stream.read(1))
    sign = ord(stream.read(1))
    num = 0
    for i in range(num_bytes):
        num += ord(stream.read(1)) * 256 ** i
    if sign == 1:
        num *= -1
    return num

def decode_large_big_ext(stream):
    num_bytes, = unpack('>L', stream.read(4))
    sign = ord(stream.read(1))
    num = 0
    for i in range(num_bytes):
        num += ord(stream.read(1)) * 256 ** i
    if sign == 1:
        num *= -1
    return num

def decode_new_reference_ext(stream):
    length, = unpack('>h', stream.read(2))
    atom = decode(stream, False)
    creation = ord(stream.read(1))
    identifiers = deque()
    for i in range(length):
        identifier, = unpack('>L', stream.read(4))
        identifiers.appendleft(identifier)
    return NewReference(atom, creation, list(identifiers))

def decode_small_atom_ext(stream):
    atom_len = ord(stream.read(1))
    atom_name = stream.read(atom_len)
    return Atom(atom_name)

def decode_fun_ext(stream):
    num_free, = unpack('>L', stream.read(4))
    pid = decode(stream, False)
    module = decode(stream, False)
    index = decode(stream, False)
    uniq = decode(stream, False)
    free_vars = []
    for i in range(num_free):
        free_var = decode(stream, False)
        free_vars.append(free_var)
    if not len(free_vars):
        free_vars = None

    return Function(pid, module, index, uniq, free_vars)

def decode_new_fun_ext(stream):
    size, = unpack('>L', stream.read(4))
    arity = ord(stream.read(1))
    uniq = stream.read(16)
    index, = unpack('>L', stream.read(4))
    num_free, = unpack('>L', stream.read(4))
    module = decode(stream, False)
    old_index = decode(stream, False)
    old_uniq = decode(stream, False)
    pid = decode(stream, False)
    free_vars = []
    for i in range(num_free):
        free_var = decode(stream, False)
        free_vars.append(free_var)
    if not len(free_vars):
        free_vars = None

    return NewFunction(
        arity, uniq, index, module, old_index, old_uniq, pid, free_vars)

def decode_export_ext(stream):
    module = decode(stream, False)
    function = decode(stream, False)
    arity = decode(stream, False)
    return Export(module, function, arity)

def decode_new_float_ext(stream):
    return unpack('>d', stream.read(8))[0]

def decode_bit_binary_ext(stream):
    length = unpack('>L', stream.read(4))[0]
    return BitBinary(ord(stream.read(1)), stream.read(length))

def decode_atom_cache_ref(stream):
    return AtomCacheRef(ord(stream.read(1)))

def decode_small_integer_ext(stream):
    return ord(stream.read(1))

def decode_integer_ext(stream):
    return unpack('>l', stream.read(4))[0]

def decode_float_ext(stream):
    return float(''.join([x for x in stream.read(31) if ord(x) > 0]))

def decode(stream, check_dist_tag=True):
    first_byte = ord(stream.read(1))
    if check_dist_tag:
        if first_byte != 131:
            raise ValueError('this is not an Erlang EXT datatype')
        else:
            ext_code = ord(stream.read(1))
    else:
        ext_code = first_byte

    if   ext_code == 70:  return decode_new_float_ext(stream)
    elif ext_code == 77:  return decode_bit_binary_ext(stream)
    elif ext_code == 82:  return decode_atom_cache_ref(stream)
    elif ext_code == 97:  return decode_small_integer_ext(stream)
    elif ext_code == 98:  return decode_integer_ext(stream)
    elif ext_code == 99:  return decode_float_ext(stream)
    elif ext_code == 100: return decode_atom_ext(stream)
    elif ext_code == 101: return decode_reference_ext(stream)
    elif ext_code == 102: return decode_port_ext(stream)
    elif ext_code == 103: return decode_pid_ext(stream)
    elif ext_code == 104: return decode_small_tuple_ext(stream)
    elif ext_code == 105: return decode_large_tuple_ext(stream)
    elif ext_code == 106: return decode_nil_ext(stream)
    elif ext_code == 107: return decode_string_ext(stream)
    elif ext_code == 108: return decode_list_ext(stream)
    elif ext_code == 109: return decode_binary_ext(stream)
    elif ext_code == 110: return decode_small_big_ext(stream)
    elif ext_code == 111: return decode_large_big_ext(stream)
    elif ext_code == 112: return decode_new_fun_ext(stream)
    elif ext_code == 113: return decode_export_ext(stream)
    elif ext_code == 114: return decode_new_reference_ext(stream)
    elif ext_code == 115: return decode_small_atom_ext(stream)
    elif ext_code == 117: return decode_fun_ext(stream)
    else:
        raise ValueError(
            'Unable to decode Erlang EXT data type: %s' % ext_code)

def encode_float(data, stream):
    stream.write(chr(70))
    stream.write(pack('>d', data))

def encode_bit_binary(data, stream):
    stream.write(chr(77))
    stream.write(pack('>L', len(data.data)))
    stream.write(chr(data.bits))
    stream.write(data.data)

def encode_atom_cache_ref(data, stream):
    stream.write(chr(82))
    stream.write(chr(data.value))

def encode_small_integer(data, stream):
    stream.write(chr(97))
    stream.write(chr(data))

def encode_integer(data, stream):
    stream.write(chr(98))
    stream.write(pack('>l', data))

def encode_long(data, stream):
    if data < 0:
        data *= -1
        sign = 1
    else:
        sign = 0
    bytes = []
    while data != 0:
        bytes.append(data & 255)
        data = data >> 8
    bytes_len = len(bytes)
    if bytes_len <= 255:
        stream.write(chr(110))
        stream.write(chr(bytes_len))
    else:
        stream.write(chr(111))
        stream.write(pack('>L', len(bytes)))
    stream.write(chr(sign))
    stream.write(''.join([chr(x) for x in bytes]))

def encode_number(data, stream):
    if 0 <= data <= 0xff:
        encode_small_integer(data, stream)
    elif -0x7fffffff - 1 <= data <= 0x7fffffff:
        encode_integer(data, stream)
    else:
        encode_long(data, stream)

def encode_atom(data, stream):
    name_len = len(data.name)
    if name_len <= 0xf:
        stream.write(chr(115))
        stream.write(chr(name_len))
    else:
        stream.write(chr(100))
        stream.write(pack('>h', name_len))
    stream.write(data.name)

def encode_reference(data, stream):
    stream.write(chr(101))
    encode(data.atom, stream, False)
    stream.write(pack('>L', data.identifier))
    stream.write(chr(data.creation))

def encode_port(data, stream):
    stream.write(chr(102))
    encode(data.atom, stream, False)
    stream.write(pack('>L', data.identifier))
    stream.write(chr(data.creation))

def encode_pid(data, stream):
    stream.write(chr(103))
    encode(data.atom, stream, False)
    stream.write(pack('>L', data.identifier))
    stream.write(pack('>L', data.serial))
    stream.write(chr(data.creation))

def encode_tuple(data, stream):
    data_len = len(data)
    if data_len < 256:
        stream.write(chr(104))
        stream.write(chr(data_len))
    else:
        stream.write(chr(105))
        stream.write(pack('>L', data_len))
    for i in range(data_len):
        encode(data[i], stream, False)

def encode_none(data, stream):
    stream.write(chr(106))

def encode_str(data, stream):
    data_len = len(data)
    if data_len > 0xffff:
        encode_list(data, stream)
    else:
        stream.write(chr(107))
        stream.write(pack('>h', data_len))
        stream.write(data)

def encode_list(data, stream):
    data_len = len(data)
    stream.write(chr(108))
    stream.write(pack('>L', data_len))
    for i in range(data_len):
        encode(data[i], stream, False)
    stream.write(chr(106))

def encode_binary(data, stream):
    stream.write(chr(109))
    stream.write(pack('>L', len(data.data)))
    stream.write(data.data)

def encode_new_reference(data, stream):
    stream.write(chr(114))
    ids_len = len(data.ids)
    stream.write(pack('>h', ids_len))
    encode(data.atom, stream, False)
    stream.write(chr(data.creation))
    for identifier in data.ids:
        stream.write(pack('>L', identifier))

def encode_function(data, stream):
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
    stream.write(chr(112))
    if data.free_vars is None:
        free_vars_len = 0
    else:
        free_vars_len = len(data.free_vars)

    bytes = StringIO()
    bytes.write(chr(data.arity))
    bytes.write(data.uniq)
    bytes.write(pack('>L', data.index))
    bytes.write(pack('>L', free_vars_len))
    encode(data.module, bytes, False)
    encode(data.old_index, bytes, False)
    encode(data.old_uniq, bytes, False)
    encode(data.pid, bytes, False)
    if free_vars_len > 0:
        for free_var in data.free_vars:
            bytes.write(pack('>L', free_var))
    bytes_value = bytes.getvalue()
    bytes.close()
    stream.write(pack('>L', len(bytes_value) + 4))
    stream.write(bytes_value)

def encode_bit_binary(data, stream):
    stream.write(chr(77))
    stream.write(pack('>L', len(data.data)))
    stream.write(chr(data.bits))
    stream.write(data.data)

def encode_export(data, stream):
    stream.write(chr(113))
    encode(data.module, stream, False)
    encode(data.function, stream, False)
    encode(data.arity, stream, False)

def encode_dict(data, stream):
    encode(zip(data.keys(), data.values()), stream, False)

def encode(data, stream, send_magic_byte=True):
    if send_magic_byte:
        stream.write(chr(131))

    data_type = type(data)
    if   data_type == float:        encode_float(data, stream)
    elif data_type == BitBinary:    encode_bit_binary(data, stream)
    elif data_type == AtomCacheRef: encode_atom_cache_ref(data, stream)
    elif data_type == int:          encode_number(data, stream)
    elif data_type == long:         encode_number(data, stream)
    elif data_type == Atom:         encode_atom(data, stream)
    elif data_type == Reference:    encode_reference(data, stream)
    elif data_type == Port:         encode_port(data, stream)
    elif data_type == Pid:          encode_pid(data, stream)
    elif data_type == tuple:        encode_tuple(data, stream)
    elif data_type == NoneType:     encode_none(data, stream)
    elif data_type == str:          encode_str(data, stream)
    elif data_type == list:         encode_list(data, stream)
    elif data_type == Binary:       encode_binary(data, stream)
    elif data_type == NewReference: encode_new_reference(data, stream)
    elif data_type == Function:     encode_function(data, stream)
    elif data_type == NewFunction:  encode_new_function(data, stream)
    elif data_type == BitBinary:    encode_bit_binary(data, stream)
    elif data_type == Export:       encode_export(data, stream)
    elif data_type == dict:         encode_dict(data, stream)
    else:
        raise ValueError('A %s is not Erlang serializable' % data_type)

class Gateway:
    in_handle = None
    out_handle = None
    stream_wrapper = None

    def __init__(self):
        if not self.__class__.in_handle:
            self.set_input(stdin)
        if not self.__class__.out_handle:
            self.set_output(stdout)
        if not self.__class__.stream_wrapper:
            self.__class__.stream_wrapper = StreamEmulator()

    def set_input(self, stream):
        self.close_input()

        if type(stream) == str:
            stream = open(stream, 'r')
        self.__class__.in_handle = stream

    def set_output(self, stream):
        self.close_output()

        if type(stream) == str:
            stream = open(stream, 'w')
        self.__class__.out_handle = stream

    def close_input(self):
        if self.__class__.in_handle:
            self.__class__.in_handle.close()

    def close_output(self):
        if self.__class__.out_handle:
            self.__class__.out_handle.close()

    def recv(self):
        message_len = self.__class__.in_handle.read(4)

        if len(message_len) < 4:
            raise ValueError('Message size payload should be 4 bytes')

        message_len, = unpack('>L', message_len)
        self.__class__.stream_wrapper.clear()
        self.__class__.stream_wrapper.write(
            self.__class__.in_handle.read(message_len))
        message = decode(self.__class__.stream_wrapper)
        return message

    def send(self, message):
        self.__class__.stream_wrapper.clear()
        encode(message, self.__class__.stream_wrapper)
        self.__class__.out_handle.write(
            pack('>L', len(self.__class__.stream_wrapper.data)))
        self.__class__.out_handle.write(self.__class__.stream_wrapper.data)
        self.__class__.out_handle.flush()
