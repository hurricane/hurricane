#!/usr/bin/env ruby

# Implementation of the Erlang binary protocol.
#
# Provides facilities to work with Standard I/O streams, sockets, and
# Erlang binary messages.

require 'socket'
require 'stringio'

# Defines the module where all Erlang-related logic will go.
module Erlang
end

# Patches older versions of Ruby to work the same way as 1.8.7+
# regarding String's bytesize() method.
if not ''.respond_to?('bytesize')
  class String
    def bytesize()
      count = 0
      each_byte() { |b| count += 1 }
      count
    end
  end
end

# Patches older versions of Ruby to work the same way as 1.8.7+
# regarding String's bytes() method.
if not ''.respond_to?('bytes')
  class String
    def bytes()
      bytes = []
      each_byte() { |b| bytes << b }
      bytes
    end
  end
end

# Patches older versions of Ruby to work the same way as 1.8.7+
# regarding StringIO's bytes() method.
if not StringIO.new().respond_to?('bytes')
  class StringIO
    def bytes()
      bytes = []
      each_byte() { |b| bytes << b }
      bytes
    end
  end
end

if "\x00\x00\x00\x01".unpack('L').eql?(1)
  Erlang::MACHINE_ENDIANNESS = 'BIG_ENDIAN'
else
  Erlang::MACHINE_ENDIANNESS = 'LITTLE_ENDIAN'
end

# Return a HEX representation of the given string.
def Erlang::hexlify(msg)
    msg.bytes().map() { |b| b.to_s(16) }.join('')
end

# Emulates a stream. Highly useful for debugging.
class Erlang::StreamEmulator
  attr_reader :data

  # Initialize the stream emulator with an optional data argument.
  def initialize(data=nil)
    @data = ''
    if not data.eql?(nil)
      write(data)
    end
    @pos = 0
  end

  # Read bytes number of data and return it. Raise an IndexError if there
  # aren't enough bytes to be read.
  def read(num)
    if @data.bytesize() < @pos + num
      raise IndexError.new("Out of data to read (was asked for #{num} byte(s), only #{data.size() - @pos} byte(s) remain)")
    end
    read_data = @data[@pos..@pos + num - 1]
    @pos += num
    read_data
  end

  # Write either binary data or a list of bytes to the stream.
  def write(data)
    if data.class().eql?(Array)
      data = data.map() { |b| b.chr() }.join('')
    end
    @data << data
  end

  # Exist for interface completeness.
  def flush()
  end

  # Reset the position and clear the data buffer.
  def clear()
    @data = ''
    @pos = 0
  end

  # Exist for interface completeness.
  def close()
  end
end

# Wraps Standard I/O input and output facilities; exposes a standard
# stream interface.
class Erlang::StdioWrapper

  # Read the specified number of bytes.
  def read(num)
    STDIN.read(num)
  end

  # Write the given data.
  def write(data)
    STDOUT.write(data)
  end

  # Override buffering and flush all data to Standard Out.
  def flush()
    STDOUT.flush()
  end

  # Exist for interface completeness.
  def close()
  end
end

# Wraps socket creation and usage logic in the standard stream interface.
class Erlang::SocketWrapper

  # Open a socket to the given host and port.
  def initialize(host, port)
    @sock = TCPSocket.new(host, port)
  end

  # Read the specified number of bytes.
  def read(num)
    chunks = []
    len_read_so_far = 0
    while len_read_so_far < num
      chunk = @sock.recv(num - len_read_so_far)
      len_read_so_far += chunk.bytesize()
      chunks << chunk
    end
    chunks.join('')
  end

  # Write the given data.
  def write(data)
    @sock.write(data)
  end

  # Exist for interface completeness.
  def flush()
  end

  # Close the socket.
  def close()
    @sock.close()
  end
end

# Implements a tuple object to be used with Erlang messaging.
class Erlang::Tuple
  attr_accessor :data

  # Set the initial data of the tuple to be the given data.
  def initialize(data)
    @data = data
  end

  # Return a human-readble representation of the object.
  def to_s()
    @data.to_s()
  end

  # Compares self to another object for equality.
  def eql?(other)
    @data.eql?(other.data)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end

  # Compare to another tuple (used mostly for sorting).
  def <=>(other)
    @data <=> other.data
  end
end

# Implements an Erlang atom cache ref.
class Erlang::AtomCacheRef
  attr_accessor :value

  # Set the given data on the object.
  def initialize(value)
    @value = value
  end

  # Return the human-readble representation.
  def to_s()
    "AtomCacheRef: #{@value}"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @value.eql?(other.value)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang atom.
class Erlang::Atom
  attr_accessor :name

  # Set the given data on the object.
  def initialize(name)
    @name = name
  end

  # Return the human-readble representation.
  def to_s()
    @name
  end

  # Compares self to another object for equality.
  def eql?(other)
    @name.eql?(other.name)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang reference.
class Erlang::Reference
  attr_accessor :atom, :identifier, :creation

  # Set the given data on the object.
  def initialize(atom, identifier, creation)
    @atom = atom
    @identifier = identifier
    @creation = creation
  end

  # Return the human-readble representation.
  def to_s()
    "#{@atom}#Ref<#{@creation}, #{@identifier}>"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @atom.eql?(other.atom) &&
    @creation.eql?(other.creation) &&
    @identifier.eql?(other.identifier)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang "new reference" (a reference created at runtime).
class Erlang::NewReference
  attr_accessor :atom, :creation, :ids

  # Set the given data on the object.
  def initialize(atom, creation, ids)
    @atom = atom
    @creation = creation
    @ids = ids
  end

  # Return the human-readble representation.
  def to_s()
    "#{@atom}#Ref<#{creation}.#{@ids.join('.')}>"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @atom.eql?(other.atom) &&
    @creation.eql?(other.creation) &&
    @ids.eql?(other.ids)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang port.
class Erlang::Port
  attr_accessor :atom, :identifier, :creation

  # Set the given data on the object.
  def initialize(atom, identifier, creation)
    @atom = atom
    @identifier = identifier
    @creation = creation
  end

  # Return the human-readble representation.
  def to_s()
    "#{@atom}#Port<#{@identifier}.#{@creation}>"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @atom.eql?(other.atom) &&
    @identifier.eql?(other.identifier) &&
    @creation.eql?(other.creation)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang pid.
class Erlang::Pid
  attr_accessor :atom, :identifier, :serial, :creation

  # Set the given data on the object.
  def initialize(atom, identifier, serial, creation)
    @atom = atom
    @identifier = identifier
    @serial = serial
    @creation = creation
  end

  # Return the human-readble representation.
  def to_s()
    "#{@atom}:<#{@serial}.#{@identifier}.#{@creation}>"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @atom.eql?(other.atom) &&
    @identifier.eql?(other.identifier) &&
    @serial.eql?(other.serial) &&
    @creation.eql?(other.creation)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang binary.
class Erlang::Binary
  attr_accessor :data

  # Set the given data on the object.
  def initialize(data)
    @data = data
  end

  # Return the human-readble representation.
  def to_s()
    "<<#{@data.bytes().map() { |b| b }.join(',')}>>"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @data.eql?(other.data)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang bit binary.
class Erlang::BitBinary
  attr_accessor :bits, :data

  # Set the given data on the object.
  def initialize(bits, data)
    @bits = bits
    @data = data
  end

  # Return the human-readble representation.
  def to_s()
    bytes = @data.bytes().to_a()
    "<<#{bytes[0..bytes.size() - 2].join(',')},#{bytes.last()}:#{@bits}>>"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @bits.eql?(other.bits) &&
    @data.eql?(other.data)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang export.
class Erlang::Export
  attr_accessor :module, :function, :arity

  # Set the given data on the object.
  def initialize(mod, function, arity)
    @module = mod
    @function = function
    @arity = arity
  end

  # Return the human-readble representation.
  def to_s()
    "#{@module}:#{@function}/#{@arity}"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @module.eql?(other.module) &&
    @function.eql?(other.function) &&
    @arity.eql?(other.arity)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang function (defined at compile-time).
class Erlang::Function
  attr_accessor :pid, :module, :index, :uniq, :free_vars

  # Set the given data on the object.
  def initialize(pid, mod, index, uniq, free_vars)
    @pid = pid
    @module = mod
    @index = index
    @uniq = uniq
    @free_vars = free_vars
  end

  # Return the human-readble representation.
  def to_s()
    "#{@module}:#{@uniq}"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @pid.eql?(other.pid) &&
    @module.eql?(other.module) &&
    @index.eql?(other.index) &&
    @uniq.eql?(other.uniq) &&
    @free_vars.eql?(other.free_vars)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Implements an Erlang function (created at run-time, usually with
# the fun () -> end syntax).
class Erlang::NewFunction
  attr_accessor :arity, :uniq, :index, :module, :old_index, :old_uniq, :pid, :free_vars

  # Set the given data on the object.
  def initialize(arity, uniq, index, mod, old_index, old_uniq, pid, free_vars)
    @arity = arity
    @uniq = uniq
    @index = index
    @module = mod
    @old_index = old_index
    @old_uniq = old_uniq
    @pid = pid
    @free_vars = free_vars
  end

  # Return the human-readble representation.
  def to_s()
    "#{@module}:#{Erlang::hexlify(@uniq)}/#{@arity}"
  end

  # Compares self to another object for equality.
  def eql?(other)
    @arity.eql?(other.arity) &&
    @uniq.eql?(other.uniq) &&
    @index.eql?(other.index) &&
    @module.eql?(other.module) &&
    @old_index.eql?(other.old_index) &&
    @old_uniq.eql?(other.old_uniq) &&
    @pid.eql?(other.pid) &&
    @free_vars.eql?(other.free_vars)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end
end

# Turn and Erlang-style property list into a hash.
def Erlang::proplist_to_hash(proplist)
  result = {}
  proplist.each do |element|
    if element.data().size() < 2
      raise ArgumentError.new(
        'Proplist elements should contain at least 2 elements'
      )
    end
    result[element.data()[0]] = element.data()[1]
  end
  result
end

# Decode and return an Erlang atom.
def Erlang::decode_atom_ext(stream)
  atom_len = stream.read(2).unpack('n')[0]
  Erlang::Atom.new(stream.read(atom_len))
end

# Decode and return an Erlang reference.
def Erlang::decode_reference_ext(stream)
  atom = Erlang::decode(stream, false)
  identifier = stream.read(4).unpack('N')[0]
  creation = stream.read(1).unpack('C')[0]
  Erlang::Reference.new(atom, identifier, creation)
end

# Decode and return an Erlang port.
def Erlang::decode_port_ext(stream)
  atom = Erlang::decode(stream, false)
  identifier = stream.read(4).unpack('N')[0]
  creation = stream.read(1).unpack('C')[0]
  Erlang::Port.new(atom, identifier, creation)
end

# Decode and return an Erlang pid.
def Erlang::decode_pid_ext(stream)
  atom = Erlang::decode(stream, false)
  identifier = stream.read(4).unpack('N')[0]
  serial = stream.read(4).unpack('N')[0]
  creation = stream.read(1).unpack('C')[0]
  Erlang::Pid.new(atom, identifier, serial, creation)
end

# Decode and return a small Erlang tuple (fewer than 256 elements).
def Erlang::decode_small_tuple_ext(stream)
  tuple_len = stream.read(1).unpack('C')[0]
  elements = []
  1.upto(tuple_len) do
    value = Erlang::decode(stream, false)
    elements << value
  end
  Erlang::Tuple.new(elements)
end

# Decode and return a large Erlang tuple (more than 256 elements).
def Erlang::decode_large_tuple_ext(stream)
  tuple_len = stream.read(4).unpack('N')[0]
  elements = []
  1.upto(tuple_len) do
    value = Erlang::decode(stream, false)
    elements << value
  end
  Erlang::Tuple.new(elements)
end

# Decode and return a nil/null/None.
def Erlang::decode_nil_ext(stream)
  return nil
end

# Decode and return a string.
def Erlang::decode_string_ext(stream)
  str_len = stream.read(2).unpack('n')[0]
  return stream.read(str_len)
end

# Decode and return a list.
#
# Depending on the list contents, a string may be returned. This will
# be the case if the list contains only byte values, which means that
# the list is actually intending to be a string, but being capped by
# Erlang's 65K char limit for strings (before they overflow into a list).
def Erlang::decode_list_ext(stream)
  list_len = stream.read(4).unpack('N')[0]
  elements = []
  is_str = true
  1.upto(list_len) do
    value = Erlang::decode(stream, false)
    is_str = is_str && value.class().eql?(Fixnum) && value < 256
    elements << value
  end
  tail = Erlang::decode(stream, false)
  if not tail.eql?(nil)
    is_str = is_str && tail.class().eql?(Fixnum) && tail < 256
    elements << tail
  end

  if is_str
    return elements.map() { |c| c.chr() }.join('')
  else
    elements
  end
end

# Decode and return an Erlang binary.
def Erlang::decode_binary_ext(stream)
  bin_len = stream.read(4).unpack('N')[0]
  Erlang::Binary.new(stream.read(bin_len))
end

# Decode and return "small" big number.
def Erlang::decode_small_big_ext(stream)
  num_bytes = stream.read(1).unpack('C')[0]
  sign = stream.read(1).unpack('C')[0]
  num = 0
  0.upto(num_bytes - 1) do |i|
    num += stream.read(1).unpack('C')[0] * 256 ** i
  end
  if sign.eql?(1)
    num *= -1
  end
  num
end

# Decode and return "large" big number.
def Erlang::decode_large_big_ext(stream)
  num_bytes = stream.read(4).unpack('N')[0]
  sign = stream.read(1).unpack('C')[0]
  num = 0
  0.upto(num_bytes - 1) do |i|
    num += stream.read(1).unpack('C')[0] * 256 ** i
  end
  if sign.eql?(1)
    num *= -1
  end
  num
end

# Decode and return an Erlang "new reference".
def Erlang::decode_new_reference_ext(stream)
  length = stream.read(2).unpack('n')[0]
  atom = Erlang::decode(stream, false)
  creation = stream.read(1).unpack('C')[0]
  identifiers = []
  1.upto(length) do
    identifier = stream.read(4).unpack('N')[0]
    identifiers.unshift(identifier)
  end
  Erlang::NewReference.new(atom, creation, identifiers)
end

# Decode and return a small Erlang atom.
def Erlang::decode_small_atom_ext(stream)
  atom_len = stream.read(1).unpack('C')[0]
  atom_name = stream.read(atom_len)
  Erlang::Atom.new(atom_name)
end

# Decode and return an Erlang function.
def Erlang::decode_fun_ext(stream)
  num_free = stream.read(4).unpack('N')[0]
  pid = Erlang::decode(stream, false)
  mod = Erlang::decode(stream, false)
  index = Erlang::decode(stream, false)
  uniq = Erlang::decode(stream, false)

  free_vars = []
  1.upto(num_free) do
    free_var = Erlang::decode(stream, false)
    free_vars << free_var
  end

  Erlang::Function.new(pid, mod, index, uniq, free_vars)
end

# Decode and return an Erlang "new function".
def Erlang::decode_new_fun_ext(stream)
  size = stream.read(4).unpack('N')[0]
  arity = stream.read(1).unpack('C')[0]
  uniq = stream.read(16)
  index = stream.read(4).unpack('N')[0]
  num_free = stream.read(4).unpack('N')[0]
  mod = Erlang::decode(stream, false)
  old_index = Erlang::decode(stream, false)
  old_uniq = Erlang::decode(stream, false)
  pid = Erlang::decode(stream, false)

  free_vars = []
  1.upto(num_free) do
    free_var = Erlang::decode(stream, false)
    free_vars << free_var
  end

  Erlang::NewFunction.new(arity, uniq, index, mod, old_index, old_uniq, pid, free_vars)
end

# Decode and return an Erlang export.
def Erlang::decode_export_ext(stream)
  mod = Erlang::decode(stream, false)
  function = Erlang::decode(stream, false)
  arity = Erlang::decode(stream, false)
  Erlang::Export.new(mod, function, arity)
end

# Decode and return an IEEE 8-byte floating-point number.
def Erlang::decode_new_float_ext(stream)
  stream.read(8).unpack('G')[0]
end

# Decode and return an Erlang bit binary.
def Erlang::decode_bit_binary_ext(stream)
  length = stream.read(4).unpack('N')[0]
  Erlang::BitBinary.new(stream.read(1).unpack('C')[0], stream.read(length))
end

# Decode and return an Erlang atom cache ref.
def Erlang::decode_atom_cache_ref(stream)
  Erlang::AtomCacheRef.new(stream.read(1).unpack('C')[0])
end

# Decode and return a small integer (byte).
def Erlang::decode_small_integer_ext(stream)
  stream.read(1).unpack('C')[0]
end

# Decode and return an integer.
def Erlang::decode_integer_ext(stream)
  bin = stream.read(4)
  if Erlang::MACHINE_ENDIANNESS.eql?('LITTLE_ENDIAN')
    bin = bin.reverse()
  end
  bin.unpack('l')[0]
end

# Decode and return a float (represented by Erlang as a string).
def Erlang::decode_float_ext(stream)
  stream.read(31).bytes().map() { |c| c.chr() }.join('').to_f()
end

# Decode a single value from the given stream and return it.
#
# If check_dist_tag, check to see that the first byte is 131 (this is
# how Erlang flags the beginning of every data type). This check does
# not need to be performed when recursively decoding nested data types,
# hence the optional argument.
def Erlang::decode(stream, check_dist_tag=true)
  first_byte = stream.read(1).unpack('C')[0]
  if check_dist_tag
    if not first_byte.eql?(131)
      raise ArgumentError.new('This is not an Erlang EXT datatype')
    else
      ext_code = stream.read(1).unpack('C')[0]
    end
  else
    ext_code = first_byte
  end

  if ext_code.eql?(70)
    return Erlang::decode_new_float_ext(stream)
  elsif ext_code.eql?(77)
    return Erlang::decode_bit_binary_ext(stream)
  elsif ext_code.eql?(82)
    return Erlang::decode_atom_cache_ref(stream)
  elsif ext_code.eql?(97)
    return Erlang::decode_small_integer_ext(stream)
  elsif ext_code.eql?(98)
    return Erlang::decode_integer_ext(stream)
  elsif ext_code.eql?(99)
    return Erlang::decode_float_ext(stream)
  elsif ext_code.eql?(100)
    return Erlang::decode_atom_ext(stream)
  elsif ext_code.eql?(101)
    return Erlang::decode_reference_ext(stream)
  elsif ext_code.eql?(102)
    return Erlang::decode_port_ext(stream)
  elsif ext_code.eql?(103)
    return Erlang::decode_pid_ext(stream)
  elsif ext_code.eql?(104)
    return Erlang::decode_small_tuple_ext(stream)
  elsif ext_code.eql?(105)
    return Erlang::decode_large_tuple_ext(stream)
  elsif ext_code.eql?(106)
    return Erlang::decode_nil_ext(stream)
  elsif ext_code.eql?(107)
    return Erlang::decode_string_ext(stream)
  elsif ext_code.eql?(108)
    return Erlang::decode_list_ext(stream)
  elsif ext_code.eql?(109)
    return Erlang::decode_binary_ext(stream)
  elsif ext_code.eql?(110)
    return Erlang::decode_small_big_ext(stream)
  elsif ext_code.eql?(111)
    return Erlang::decode_large_big_ext(stream)
  elsif ext_code.eql?(112)
    return Erlang::decode_new_fun_ext(stream)
  elsif ext_code.eql?(113)
    return Erlang::decode_export_ext(stream)
  elsif ext_code.eql?(114)
    return Erlang::decode_new_reference_ext(stream)
  elsif ext_code.eql?(115)
    return Erlang::decode_small_atom_ext(stream)
  elsif ext_code.eql?(117)
    return Erlang::decode_fun_ext(stream)
  else
    raise TypeError.new("Unable to decode Erlang EXT data type: #{ext_code}")
  end
end

# Encode a floating-point number into the stream.
def Erlang::encode_float(data, stream)
  stream.write(70.chr())
  stream.write([data].pack('G'))
end

# Encode an Erlang atom cache ref into the stream.
def Erlang::encode_atom_cache_ref(data, stream)
  stream.write(82.chr())
  stream.write(data.value.chr())
end

# Encode a small integer (byte) into the stream.
def Erlang::encode_small_integer(data, stream)
  stream.write(97.chr())
  stream.write(data.chr())
end

# Encode an integer into the stream.
def Erlang::encode_integer(data, stream)
  stream.write(98.chr())
  bin = [data].pack('L')
  if Erlang::MACHINE_ENDIANNESS.eql?('LITTLE_ENDIAN')
    bin = bin.reverse()
  end
  stream.write(bin)
end

# Encode a large number into the stream.
def Erlang::encode_long(data, stream)
  if data < 0
    data *= -1
    sign = 1
  else
    sign = 0
  end
  bytes = []
  while data != 0
    bytes << (data & 255)
    data = data >> 8
  end

  bytes_len = bytes.size()
  if bytes_len <= 255
    stream.write(110.chr())
    stream.write(bytes_len.chr())
  else
    stream.write(111.chr())
    stream.write([bytes.size()].pack('N'))
  end
  stream.write(sign.chr())
  stream.write(bytes.map() { |b| b.chr() }.join(''))
end

# Encode any-size number into the stream.
def Erlang::encode_number(data, stream)
  if 0 <= data && data <= 0xff
    encode_small_integer(data, stream)
  elsif -0x7fffffff - 1 <= data && data <= 0x7fffffff
    encode_integer(data, stream)
  else
    encode_long(data, stream)
  end
end

# Encode an Erlang atom into the stream.
def Erlang::encode_atom(data, stream)
  name_len = data.name.bytesize()
  if name_len <= 0xf
    stream.write(115.chr())
    stream.write(name_len.chr())
  else
    stream.write(100.chr())
    stream.write([name_len].pack('n'))
  end
  stream.write(data.name)
end

# Encode an Erlang reference into the stream.
def Erlang::encode_reference(data, stream)
  stream.write(101.chr())
  Erlang::encode(data.atom, stream, false)
  stream.write([data.identifier].pack('N'))
  stream.write(data.creation.chr())
end

# Encode an Erlang port into the stream.
def Erlang::encode_port(data, stream)
  stream.write(102.chr())
  Erlang::encode(data.atom, stream, false)
  stream.write([data.identifier].pack('N'))
  stream.write(data.creation.chr())
end

# Encode an Erlang pid into the stream.
def Erlang::encode_pid(data, stream)
  stream.write(103.chr())
  Erlang::encode(data.atom, stream, false)
  stream.write([data.identifier].pack('N'))
  stream.write([data.serial].pack('N'))
  stream.write(data.creation.chr())
end

# Encode a tuple into the stream.
def Erlang::encode_tuple(data, stream)
  data_len = data.data.size()
  if data_len < 256
    stream.write(104.chr())
    stream.write(data_len.chr())
  else
    stream.write(105.chr())
    stream.write([data_len].pack('N'))
  end
  0.upto(data_len - 1) do |i|
    Erlang::encode(data.data[i], stream, false)
  end
end

# Encode a NoneType into the stream (as Erlang nil).
def Erlang::encode_none(data, stream)
  stream.write(106.chr())
end

# Encode a string into the stream.
def Erlang::encode_str(data, stream)
  data_len = data.bytesize()
  if data_len > 0xffff
    encode_list(data, stream)
  else
    stream.write(107.chr())
    stream.write([data_len].pack('n'))
    stream.write(data)
  end
end

# Encode a list into the stream.
def Erlang::encode_list(data, stream)
  data_len = data.size()
  stream.write(108.chr())
  stream.write([data_len].pack('N'))
  0.upto(data_len - 1) do |i|
    Erlang::encode(data[i], stream, false)
  end
  stream.write(106.chr())
end

# Encode an Erlang binary into the stream.
def Erlang::encode_binary(data, stream)
  stream.write(109.chr())
  stream.write([data.data.size()].pack('N'))
  stream.write(data.data)
end

# Encode an Erlang new reference into the stream.
def Erlang::encode_new_reference(data, stream)
  stream.write(114.chr())
  ids_len = data.ids.size()
  stream.write([ids_len].pack('n'))
  Erlang::encode(data.atom, stream, false)
  stream.write(data.creation.chr())
  data.ids.each() do |identifier|
    stream.write([identifier].pack('N'))
  end
end

# Encode an Erlang function into the stream.
def Erlang::encode_function(data, stream)
  stream.write(117.chr())
  free_vars_len = data.free_vars.size()
  stream.write([free_vars_len].pack('N'))
  Erlang::encode(data.pid, stream, false)
  Erlang::encode(data.module, stream, false)
  Erlang::encode(data.index, stream, false)
  Erlang::encode(data.uniq, stream, false)
  if free_vars_len > 0
    data.free_vars.each() do |free_var|
      stream.write([free_var].pack('N'))
    end
  end
end

# Encode an Erlang "new function" into the stream.
def Erlang::encode_new_function(data, stream)
  stream.write(112.chr())
  free_vars_len = data.free_vars.size()
  bytes = StringIO.new()
  bytes.write(data.arity.chr())
  bytes.write(data.uniq)
  bytes.write([data.index].pack('N'))
  bytes.write([free_vars_len].pack('N'))
  Erlang::encode(data.module, bytes, false)
  Erlang::encode(data.old_index, bytes, false)
  Erlang::encode(data.old_uniq, bytes, false)
  Erlang::encode(data.pid, bytes, false)
  if free_vars_len > 0
    data.free_vars.each() do |free_var|
      stream.write([free_var].pack('N'))
    end
  end
  bytes.rewind()
  bytes_value = bytes.bytes().to_a()
  bytes.close()
  stream.write([bytes_value.size() + 4].pack('N'))
  stream.write(bytes_value)
end

# Encode an Erlang bit binary into the stream.
def Erlang::encode_bit_binary(data, stream)
  stream.write(77.chr())
  stream.write([data.data.size()].pack('N'))
  stream.write(data.bits.chr())
  stream.write(data.data)
end

# Encode an Erlang export into the stream.
def Erlang::encode_export(data, stream)
  stream.write(113.chr())
  Erlang::encode(data.module, stream, false)
  Erlang::encode(data.function, stream, false)
  Erlang::encode(data.arity, stream, false)
end

# Encode a hash into the stream (as a property list).
def Erlang::encode_hash(data, stream)
  proplist = []
  data.each_pair() do |key, value|
    proplist << Erlang::Tuple.new([key, value])
  end

  # sorting is done to get consistency between Ruby 1.8.x and Ruby 1.9.x
  proplist = proplist.sort()

  Erlang::encode(proplist, stream, false)
end

# Encode the given data into the given stream.
#
# If send_magic_byte, the value 131 is sent before anything (this is
# how Erlang denotes that there is a new piece of data coming across).
# However, for nested data, this only needs to be sent once, hence
# the optional argument.
def Erlang::encode(data, stream, send_magic_byte=true)
  if send_magic_byte
    stream.write(131.chr())
  end

  data_type = data.class()
  if data_type.eql?(Float)
    Erlang::encode_float(data, stream)
  elsif data_type.eql?(Erlang::AtomCacheRef)
    Erlang::encode_atom_cache_ref(data, stream)
  elsif data_type.eql?(Fixnum)
    Erlang::encode_number(data, stream)
  elsif data_type.eql?(Bignum)
    Erlang::encode_number(data, stream)
  elsif data_type.eql?(Erlang::Atom)
    Erlang::encode_atom(data, stream)
  elsif data_type.eql?(Erlang::Reference)
    Erlang::encode_reference(data, stream)
  elsif data_type.eql?(Erlang::Port)
    Erlang::encode_port(data, stream)
  elsif data_type.eql?(Erlang::Pid)
    Erlang::encode_pid(data, stream)
  elsif data_type.eql?(Erlang::Tuple)
    Erlang::encode_tuple(data, stream)
  elsif data_type.eql?(NilClass)
    Erlang::encode_none(data, stream)
  elsif data_type.eql?(String)
    Erlang::encode_str(data, stream)
  elsif data.kind_of?(Array)
    Erlang::encode_list(data, stream)
  elsif data_type.eql?(Erlang::Binary)
    Erlang::encode_binary(data, stream)
  elsif data_type.eql?(Erlang::NewReference)
    Erlang::encode_new_reference(data, stream)
  elsif data_type.eql?(Erlang::Function)
    Erlang::encode_function(data, stream)
  elsif data_type.eql?(Erlang::NewFunction)
    Erlang::encode_new_function(data, stream)
  elsif data_type.eql?(Erlang::BitBinary)
    Erlang::encode_bit_binary(data, stream)
  elsif data_type.eql?(Erlang::Export)
    Erlang::encode_export(data, stream)
  elsif data.kind_of?(Hash)
    Erlang::encode_hash(data, stream)
  elsif data.respond_to?('to_erlang')
    Erlang::encode(data.to_erlang(), stream, false)
  else
    raise TypeError.new("A #{data_type} is not Erlang serializable")
  end
end

# Implements a class that can be used to conveniently interface with
# Hurricane to send/receive messages.
class Erlang::Gateway

  # Initialize with an optional stream. If no stream is provided,
  # Standard I/O will be used.
  def initialize(stream)
    if stream.eql?(nil)
      @stream = Erlang::StdioWrapper.new()
    else
      @stream = stream
    end
    @stream_wrapper = Erlang::StreamEmulator.new()
  end

  # Close any open stream and set the new one.
  def stream=(stream)
    close()
    @stream = stream
  end

  # If there is an active stream, close it.
  def close()
    if not @stream.eql?(nil)
      @stream.close()
    end
  end

  # Receive one message from Hurricane.
  def do_recv()
    message_len = @stream.read(4)

    if message_len.bytesize() < 4
      raise IOError.new('Message size payload should be 4 bytes')
    end

    message_len = message_len.unpack('N')[0]
    @stream_wrapper.clear()
    @stream_wrapper.write(@stream.read(message_len))
    Erlang::decode(@stream_wrapper)
  end

  # Send one message to Hurricane.
  def do_send(message)
    @stream_wrapper.clear()
    Erlang::encode(message, @stream_wrapper)
    @stream.write([@stream_wrapper.data.bytesize()].pack('N'))
    @stream.write(@stream_wrapper.data)
    @stream.flush()
  end
end
