#!/usr/bin/env ruby

require 'socket'
require 'stringio'

module Erlang
end

if "\x00\x00\x00\x01".unpack('L').eql?(1)
  Erlang::MACHINE_ENDIANNESS = 'BIG_ENDIAN'
else
  Erlang::MACHINE_ENDIANNESS = 'LITTLE_ENDIAN'
end

def Erlang::hexlify(msg)
    msg.bytes().map() { |b| b.to_s(16) }.join('')
end

class Erlang::StreamEmulator
  attr_reader :data

  def initialize(data=nil)
    @data = ''
    if not data.eql?(nil)
      write(data)
    end
    @pos = 0
  end

  def read(num)
    data = @data.bytes().to_a()
    if data.size() < @pos + num
      raise IndexError.new("Out of data to read (was asked for #{num} byte(s), only #{data.size() - pos} byte(s) remain)")
    end
    read_data = data[@pos..@pos + num - 1]
    @pos += num
    read_data.map() { |c| c.chr() }.join('')
  end

  def write(data)
    if data.class().eql?(Array)
      data = data.map() { |b| b.chr() }.join('')
    end
    @data << data
  end

  def flush()
  end

  def clear()
    data = ''
    pos = 0
  end

  def close()
  end
end

class Erlang::StdioWrapper
  def read(num)
    STDIN.read(num)
  end

  def write(data)
    STDOUT.write(data)
  end

  def flush()
    STDOUT.flush()
  end

  def close()
  end
end

class Erlang::SocketWrapper
  def initialize(host, port)
    @sock = TCPSocket.new(host, port)
  end

  def read(num)
    @sock.recv(num)
  end

  def write(data)
    @sock.write(data)
  end

  def flush()
  end

  def close()
    @sock.close()
  end
end

class Erlang::Tuple
  attr_accessor :data

  def initialize(data)
    @data = data
  end

  def to_s()
    @data.to_s()
  end

  def eql?(other)
    @data.eql?(other.data)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::AtomCacheRef
  attr_accessor :value

  def initialize(value)
    @value = value
  end

  def to_s()
    "AtomCacheRef: #{@value}"
  end

  def eql?(other)
    @value.eql?(other.value)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Atom
  attr_accessor :name

  def initialize(name)
    @name = name
  end

  def to_s()
    @name
  end

  def eql?(other)
    @name.eql?(other.name)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Reference
  attr_accessor :atom, :identifier, :creation

  def initialize(atom, identifier, creation)
    @atom = atom
    @identifier = identifier
    @creation = creation
  end

  def to_s()
    "#{@atom}#Ref<#{@creation}, #{@identifier}>"
  end

  def eql?(other)
    @atom.eql?(other.atom) &&
    @creation.eql?(other.creation) &&
    @identifier.eql?(other.identifier)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::NewReference
  attr_accessor :atom, :creation, :ids

  def initialize(atom, creation, ids)
    @atom = atom
    @creation = creation
    @ids = ids
  end

  def to_s()
    "#{@atom}#Ref<#{creation}.#{@ids.join('.')}>"
  end

  def eql?(other)
    @atom.eql?(other.atom) &&
    @creation.eql?(other.creation) &&
    @ids.eql?(other.ids)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Port
  attr_accessor :atom, :identifier, :creation

  def initialize(atom, identifier, creation)
    @atom = atom
    @identifier = identifier
    @creation = creation
  end

  def to_s()
    "#{@atom}#Port<#{@identifier}.#{@creation}>"
  end

  def eql?(other)
    @atom.eql?(other.atom) &&
    @identifier.eql?(other.identifier) &&
    @creation.eql?(other.creation)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Pid
  attr_accessor :atom, :identifier, :serial, :creation

  def initialize(atom, identifier, serial, creation)
    @atom = atom
    @identifier = identifier
    @serial = serial
    @creation = creation
  end

  def to_s()
    "#{@atom}:<#{@serial}.#{@identifier}.#{@creation}>"
  end

  def eql?(other)
    @atom.eql?(other.atom) &&
    @identifier.eql?(other.identifier) &&
    @serial.eql?(other.serial) &&
    @creation.eql?(other.creation)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Binary
  attr_accessor :data

  def initialize(data)
    @data = data
  end

  def to_s()
    "<<#{@data.bytes().map() { |b| b }.join(',')}>>"
  end

  def eql?(other)
    @data.eql?(other.data)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::BitBinary
  attr_accessor :bits, :data

  def initialize(bits, data)
    @bits = bits
    @data = data
  end

  def to_s()
    bytes = @data.bytes().to_a()
    "<<#{bytes[0..bytes.size() - 2].join(',')},#{bytes.last()}:#{@bits}>>"
  end

  def eql?(other)
    @bits.eql?(other.bits) &&
    @data.eql?(other.data)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Export
  attr_accessor :module, :function, :arity

  def initialize(mod, function, arity)
    @module = mod
    @function = function
    @arity = arity
  end

  def to_s()
    "#{@module}:#{@function}/#{@arity}"
  end

  def eql?(other)
    @module.eql?(other.module) &&
    @function.eql?(other.function) &&
    @arity.eql?(other.arity)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::Function
  attr_accessor :pid, :module, :index, :uniq, :free_vars

  def initialize(pid, mod, index, uniq, free_vars)
    @pid = pid
    @module = mod
    @index = index
    @uniq = uniq
    @free_vars = free_vars
  end

  def to_s()
    "#{@module}:#{@uniq}"
  end

  def eql?(other)
    @pid.eql?(other.pid) &&
    @module.eql?(other.module) &&
    @index.eql?(other.index) &&
    @uniq.eql?(other.uniq) &&
    @free_vars.eql?(other.free_vars)
  end

  def ==(other)
    eql?(other)
  end
end

class Erlang::NewFunction
  attr_accessor :arity, :uniq, :index, :module, :old_index, :old_uniq, :pid, :free_vars

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

  def to_s()
    "#{@module}:#{Erlang::hexlify(@uniq)}/#{@arity}"
  end

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

  def ==(other)
    eql?(other)
  end
end

def Erlang::decode_atom_ext(stream)
  atom_len = stream.read(2).unpack('n')[0]
  Erlang::Atom.new(stream.read(atom_len))
end

def Erlang::decode_reference_ext(stream)
  atom = Erlang::decode(stream, false)
  identifier = stream.read(4).unpack('N')[0]
  creation = stream.read(1)[0]
  Erlang::Reference.new(atom, identifier, creation)
end

def Erlang::decode_port_ext(stream)
  atom = Erlang::decode(stream, false)
  identifier = stream.read(4).unpack('N')[0]
  creation = stream.read(1)[0]
  Erlang::Port.new(atom, identifier, creation)
end

def Erlang::decode_pid_ext(stream)
  atom = Erlang::decode(stream, false)
  identifier = stream.read(4).unpack('N')[0]
  serial = stream.read(4).unpack('N')[0]
  creation = stream.read(1)[0]
  Erlang::Pid.new(atom, identifier, serial, creation)
end

def Erlang::decode_small_tuple_ext(stream)
  tuple_len = stream.read(1)[0]
  elements = []
  1.upto(tuple_len) do
    value = decode(stream, false)
    elements << value
  end
  Erlang::Tuple.new(elements)
end

def Erlang::decode_large_tuple_ext(stream)
  tuple_len = stream.read(4).unpack('N')[0]
  elements = []
  1.upto(tuple_len) do
    value = decode(stream, false)
    elements << value
  end
  Erlang::Tuple.new(elements)
end

def Erlang::decode_nil_ext(stream)
  return nil
end

def Erlang::decode_string_ext(stream)
  str_len = stream.read(2).unpack('n')[0]
  return stream.read(str_len)
end

def Erlang::decode_list_ext(stream)
  list_len = stream.read(4).unpack('N')[0]
  elements = []
  is_str = true
  1.upto(list_len) do
    value = decode(stream, false)
    is_str = is_str && value.class().eql?(Fixnum) && value < 256
    elements << value
  end
  tail = decode(stream, false)
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

def Erlang::decode_binary_ext(stream)
    bin_len = stream.read(4).unpack('N')[0]
    Erlang::Binary.new(stream.read(bin_len))
end

def Erlang::decode_small_big_ext(stream)
  num_bytes = stream.read(1)[0]
  sign = stream.read(1)[0]
  num = 0
  0.upto(num_bytes - 1) do |i|
    num += stream.read(1)[0] * 256 ** i
  end
  if sign.eql?(1)
    num *= -1
  end
  num
end

def Erlang::decode_large_big_ext(stream)
  num_bytes = stream.read(4).unpack('N')[0]
  sign = stream.read(1)[0]
  num = 0
  0.upto(num_bytes - 1) do |i|
    num += stream.read(1)[0] * 256 ** i
  end
  if sign.eql?(1)
    num *= -1
  end
  num
end

def Erlang::decode_new_reference_ext(stream)
  length = stream.read(2).unpack('n')[0]
  atom = decode(stream, false)
  creation = stream.read(1)[0]
  identifiers = []
  1.upto(length) do
    identifier = stream.read(4).unpack('N')[0]
    identifiers.unshift(identifier)
  end
  Erlang::NewReference.new(atom, creation, identifiers)
end

def Erlang::decode_small_atom_ext(stream)
  atom_len = stream.read(1)[0]
  atom_name = stream.read(atom_len)
  Erlang::Atom.new(atom_name)
end

def Erlang::decode_fun_ext(stream)
  num_free = stream.read(4).unpack('N')[0]
  pid = decode(stream, false)
  mod = decode(stream, false)
  index = decode(stream, false)
  uniq = decode(stream, false)

  free_vars = []
  1.upto(num_free) do
    free_var = decode(stream, false)
    free_vars << free_var
  end

  Erlang::Function.new(pid, mod, index, uniq, free_vars)
end

def Erlang::decode_new_fun_ext(stream)
  size = stream.read(4).unpack('N')[0]
  arity = stream.read(1)[0]
  uniq = stream.read(16)
  index = stream.read(4).unpack('N')[0]
  num_free = stream.read(4).unpack('N')[0]
  mod = decode(stream, false)
  old_index = decode(stream, false)
  old_uniq = decode(stream, false)
  pid = decode(stream, false)

  free_vars = []
  1.upto(num_free) do
    free_var = decode(stream, false)
    free_vars << free_var
  end

  Erlang::NewFunction.new(arity, uniq, index, mod, old_index, old_uniq, pid, free_vars)
end

def Erlang::decode_export_ext(stream)
  mod = decode(stream, false)
  function = decode(stream, false)
  arity = decode(stream, false)
  Erlang::Export.new(mod, function, arity)
end

def Erlang::decode_new_float_ext(stream)
  stream.read(8).unpack('G')[0]
end

def Erlang::decode_bit_binary_ext(stream)
  length = stream.read(4).unpack('N')[0]
  Erlang::BitBinary.new(stream.read(1)[0], stream.read(length))
end

def Erlang::decode_atom_cache_ref(stream)
    Erlang::AtomCacheRef.new(stream.read(1)[0])
end

def Erlang::decode_small_integer_ext(stream)
  stream.read(1)[0]
end

def Erlang::decode_integer_ext(stream)
  bin = stream.read(4)
  if Erlang::MACHINE_ENDIANNESS.eql?('LITTLE_ENDIAN')
    bin = bin.reverse()
  end
  bin.unpack('l')[0]
end

def Erlang::decode_float_ext(stream)
    stream.read(31).bytes().map() { |c| c.chr() }.join('').to_f()
end

def Erlang::decode(stream, check_dist_tag=true)
  first_byte = stream.read(1)[0]
  if check_dist_tag
    if not first_byte.eql?(131)
      raise ArgumentError('This is not an Erlang EXT datatype')
    else
      ext_code = stream.read(1)[0]
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

def Erlang::encode_float(data, stream)
  stream.write(70.chr())
  stream.write([data].pack('G'))
end

def Erlang::encode_bit_binary(data, stream)
  stream.write((77.chr()))
  stream.write([data.data.size()].pack('N'))
  stream.write(data.bits.chr())
  stream.write(data.data)
end

def Erlang::encode_atom_cache_ref(data, stream)
  stream.write(82.chr())
  stream.write(data.value.chr())
end

def Erlang::encode_small_integer(data, stream)
  stream.write(97.chr())
  stream.write(data.chr())
end

def Erlang::encode_integer(data, stream)
  stream.write(98.chr())
  bin = [data].pack('L')
  if Erlang::MACHINE_ENDIANNESS.eql?('LITTLE_ENDIAN')
    bin = bin.reverse()
  end
  stream.write(bin)
end

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

def Erlang::encode_number(data, stream)
  if 0 <= data && data <= 0xff
    encode_small_integer(data, stream)
  elsif -0x7fffffff - 1 <= data && data <= 0x7fffffff
    encode_integer(data, stream)
  else
    encode_long(data, stream)
  end
end

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

def Erlang::encode_reference(data, stream)
  stream.write(101.chr())
  encode(data.atom, stream, false)
  stream.write([data.identifier].pack('N'))
  stream.write(data.creation.chr())
end

def Erlang::encode_port(data, stream)
  stream.write(102.chr())
  encode(data.atom, stream, false)
  stream.write([data.identifier].pack('N'))
  stream.write(data.creation.chr())
end

def Erlang::encode_pid(data, stream)
  stream.write(103.chr())
  encode(data.atom, stream, false)
  stream.write([data.identifier].pack('N'))
  stream.write([data.serial].pack('N'))
  stream.write(data.creation.chr())
end

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
    encode(data.data[i], stream, false)
  end
end

def Erlang::encode_none(data, stream)
  stream.write(106.chr())
end

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

def Erlang::encode_list(data, stream)
  data_len = data.size()
  stream.write(108.chr())
  stream.write([data_len].pack('N'))
  0.upto(data_len - 1) do |i|
    encode(data[i], stream, false)
  end
  stream.write(106.chr())
end

def Erlang::encode_binary(data, stream)
  stream.write(109.chr())
  stream.write([data.data.size()].pack('N'))
  stream.write(data.data)
end

def Erlang::encode_new_reference(data, stream)
  stream.write(114.chr())
  ids_len = data.ids.size()
  stream.write([ids_len].pack('n'))
  encode(data.atom, stream, false)
  stream.write(data.creation.chr())
  data.ids.each() do |identifier|
    stream.write([identifier].pack('N'))
  end
end

def Erlang::encode_function(data, stream)
  stream.write(117.chr())
  free_vars_len = data.free_vars.size()
  stream.write([free_vars_len].pack('N'))
  encode(data.pid, stream, false)
  encode(data.module, stream, false)
  encode(data.index, stream, false)
  encode(data.uniq, stream, false)
  if free_vars_len > 0:
    data.free_vars.each() do |free_var|
      stream.write([free_var].pack('N'))
    end
  end
end

def Erlang::encode_new_function(data, stream)
  stream.write(112.chr())
  free_vars_len = data.free_vars.size()
  bytes = StringIO.new()
  bytes.write(data.arity.chr())
  bytes.write(data.uniq)
  bytes.write([data.index].pack('N'))
  bytes.write([free_vars_len].pack('N'))
  encode(data.module, bytes, false)
  encode(data.old_index, bytes, false)
  encode(data.old_uniq, bytes, false)
  encode(data.pid, bytes, false)
  if free_vars_len > 0:
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

def Erlang::encode_bit_binary(data, stream)
  stream.write(77.chr())
  stream.write([data.data.size()].pack('N'))
  stream.write(data.bits.chr())
  stream.write(data.data)
end

def Erlang::encode_export(data, stream)
  stream.write(113.chr())
  encode(data.module, stream, false)
  encode(data.function, stream, false)
  encode(data.arity, stream, false)
end

def Erlang::encode_hash(data, stream)
  proplist = []
  data.each_pair() do |key, value|
    proplist << Erlang::Tuple.new([key, value])
  end
  Erlang::encode(proplist, stream, false)
end

def Erlang::encode(data, stream, send_magic_byte=true)
  if send_magic_byte
    stream.write(131.chr())
  end

  data_type = data.class()
  if data_type.eql?(Float)
    Erlang::encode_float(data, stream)
  elsif data_type.eql?(Erlang::BitBinary)
    Erlang::encode_bit_binary(data, stream)
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
  elsif data_type.eql?(Array)
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
  elsif data_type.eql?(Hash)
    Erlang::encode_hash(data, stream)
  else
    raise TypeError.new("A #{data_type} is not Erlang serializable")
  end
end

class Erlang::Gateway
  def initialize(stream)
    if stream.eql?(nil)
      @stream = Erlang::StdioWrapper.new()
    else
      @stream = stream
    end
    @stream_wrapper = Erlang::StreamEmulator.new()
  end

  def stream=(stream)
    close()
    @stream = stream
  end

  def close()
    if not @stream.eql?(nil)
      @stream.close()
    end
  end

  def recv()
    message_len = @stream.read(4)

    if message_len.bytesize() < 4
      raise IOError.new('Message size payload should be 4 bytes')
    end

    message_len = message_len.unpack('N')[0]
    @stream_wrapper.clear()
    @stream_wrapper.write(@stream.read(message_len))
    Erlang::decode(@stream_wrapper)
  end

  def send(message)
    @stream_wrapper.clear()
    Erlang::encode(message, @stream_wrapper)
    @stream.write([@stream_wrapper.data.bytesize()].pack('N'))
    @stream.write(@stream_wrapper.data)
    @stream.flush()
  end
end
