"""
Convenience utilities for use with Hurricane.

Provides facilities to more easily work with the messaging system.
"""

from hurricane import erl_codec


class Message(object):
    """Represents a standard Hurricane message."""

    def __init__(self):
        """Initialize the payload to be dummy values."""
        self.type = ''
        self.destination = ''
        self.tag = ''
        self.data = ''
        self.timeout = 10000

    def __str__(self):
        """Return the human-readble representation."""
        return 'Message: %s' % vars(self)

    def __repr__(self):
        """Return the Pythonic representation."""
        return 'Message(%s)' % vars(self)


class Gateway(erl_codec.Gateway):
    """
    Extends the Erlang Gateway to provide Hurricane-specific
    functionality.
    """

    def __init__(self, *args, **kwargs):
        """Pass along all arguments to the parent class."""
        super(Gateway, self).__init__(*args, **kwargs)

    def register_server(self, name):
        """Register with a named group in the Hurricane system."""
        self.send((
            erl_codec.Atom('register_with_group'),
            erl_codec.Atom(name)))

    def send_ready_signal(self):
        """
        When running over Standard I/O, indicate that the process has
        successfully started up and is ready to receive requests.
        """
        self.send((erl_codec.Atom('ready'), ))

    def recv(self):
        """
        Receive the next Hurricane message, turn it into a Message
        object.
        """
        data = super(Gateway, self).recv()
        message = Message()
        message.type = data[0].name
        message.destination = data[1]
        message.tag = data[2]
        message.data = data[3]
        return message

    def send(self, message):
        """
        Turn a message object into a Hurricane message and send it.
        """
        if type(message) == Message:
            destination = message.destination
            if isinstance(destination, basestring):
                destination = erl_codec.Atom(destination)

            data = (
                erl_codec.Atom(message.type),
                destination,
                message.tag,
                message.data,
                message.timeout)
        else:
            data = message

        super(Gateway, self).send(data)
