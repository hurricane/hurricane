require 'erl_codec'

# Defines the module where all Hurricane-related logic will go.
module Hurricane
end

# Implements a message type that can be used to message easily with
# Hurricane.
class Hurricane::Message

  attr_accessor :type, :destination, :tag, :data, :timeout

  # Initializes the object with default values.
  def initialize()
    @type = ''
    @destination = ''
    @tag = ''
    @data = ''
    @timeout = 10000
  end

  # Return a human-readble representation of the object.
  def to_s()
    {
      'type' => @type,
      'destination' => @destination,
      'tag' => @tag,
      'data' => @data,
      'timeout' => @timeout
    }.to_s()
  end

  # Compares self to another object for equality.
  def eql?(other)
    @type.eql?(other.type) &&
    @destination.eql?(other.destination) &&
    @tag.eql?(other.tag) &&
    @data.eql?(other.data) &&
    @timeout.eql?(other.timeout)
  end

  # Uses eql?() to compare self to another object for equality.
  def ==(other)
    eql?(other)
  end

end


# Extends the Erlang Gateway to provide Hurricane-specific
# functionality.
class Hurricane::Gateway < Erlang::Gateway

  # Pass along all arguments to the parent class.
  def initialize(*args)
    super(*args)
  end

  # Register with a named group in the Hurricane system.
  def register_server(name)
    do_send(Erlang::Tuple.new([
      Erlang::Atom.new('register_with_group'),
      Erlang::Atom.new(name)]))
  end

  # When running over Standard I/O, indicate that the process has
  # successfully started up and is ready to receive requests.
  def send_ready_signal()
    do_send(Erlang::Tuple.new([Erlang::Atom.new('ready')]))
  end

  # Receive the next Hurricane message, turn it into a Message object.
  def do_recv()
    data = super()
    message = Hurricane::Message.new()
    message.type = data.data[0].name
    message.destination = data.data[1]
    message.tag = data.data[2]
    message.data = data.data[3]
    message
  end

  # Turn a message object into a Hurricane message and send it.
  def do_send(message)
    if message.instance_of?(Hurricane::Message)
      destination = message.destination
      if destination.instance_of?(String)
        destination = Erlang::Atom.new(destination)
      end

      data = Erlang::Tuple.new([
          Erlang::Atom.new(message.type),
          destination,
          message.tag,
          message.data,
          message.timeout])
    else
      data = message
    end

    super(data)
  end

end

