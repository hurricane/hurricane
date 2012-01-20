<?php

/**
 * Implementation of the Hurricane messaging utilities.
 *
 * Provides a way to deal with the gateway conveniently and a way to
 * represent messages.
 */
namespace Hurricane;

require_once dirname(__FILE__) . '/erl_codec.php';

class Message
{
    /**
     * The type of message being sent/received.
     *
     * @var string
     */
    private $_type;

    /**
     * The source/destination of the message. Can be a string or an
     * atom.
     *
     * @var mixed
     */
    private $_destination;

    /**
     * The message tag--used for keeping messages in order.
     *
     * @var mixed
     */
    private $_tag;

    /**
     * The message payload.
     *
     * @var mixed
     */
    private $_data;

    /**
     * The timeout of the message.
     *
     * @var integer
     */
    private $_timeout;

    /**
     * Constructs a new Hurricane message with default values.
     *
     * @return void
     */
    public function __construct()
    {
        $this->_type = '';
        $this->_destination = '';
        $this->_tag = '';
        $this->_data = '';
        $this->_timeout = 10000;
    }

    /**
     * Facilitates the fluent API.
     *
     * @return Message A new message.
     */
    public static function create()
    {
        return new self();
    }

    /**
     * Getter for the type.
     *
     * @return string
     */
    public function getType()
    {
        return $this->_type;
    }

    /**
     * Setter for the type.
     *
     * @param string $type The type of the message.
     *
     * @return Message The object, for a fluent API.
     */
    public function setType($type)
    {
        $this->_type = $type;
        return $this;
    }

    /**
     * Getter for the source/destination.
     *
     * @return mixed
     */
    public function getDestination()
    {
        return $this->_destination;
    }

    /**
     * Setter for the destination.
     *
     * @param mixed $destination The destination of the message. Can be
     * a string or an atom.
     *
     * @return Message The object, for a fluent API.
     */
    public function setDestination($destination)
    {
        $this->_destination = $destination;
        return $this;
    }

    /**
     * Getter for the tag.
     *
     * @return mixed
     */
    public function getTag()
    {
        return $this->_tag;
    }

    /**
     * Setter for the tag.
     *
     * @param mixed $tag The tag of the message.
     *
     * @return Message The object, for a fluent API.
     */
    public function setTag($tag)
    {
        $this->_tag = $tag;
        return $this;
    }

    /**
     * Getter for the data.
     *
     * @return mixed
     */
    public function getData()
    {
        return $this->_data;
    }

    /**
     * Setter for the data.
     *
     * @param mixed $data The data of the message.
     *
     * @return Message The object, for a fluent API.
     */
    public function setData($data)
    {
        $this->_data = $data;
        return $this;
    }

    /**
     * Getter for the timeout.
     *
     * @return integer
     */
    public function getTimeout()
    {
        return $this->_timeout;
    }

    /**
     * Setter for the timeout.
     *
     * @param mixed $timeout The timeout of the message.
     *
     * @return Message The object, for a fluent API.
     */
    public function setTimeout($timeout)
    {
        $this->_timeout = $timeout;
        return $this;
    }
}

/**
 * Extends the Erlang Gateway to provide Hurricane-specific
 * functionality.
 */
class Gateway extends \Erlang\Gateway
{
    /**
     * Initialize with an optional stream. If no stream is provided,
     * Standard I/O will be used.
     *
     * @param \Erlang\StreamInterface $stream
     *
     * @return void
     */
    public function __construct(\Erlang\StreamInterface $stream=null)
    {
        parent::__construct($stream);
    }

    /**
     * Register with a named group in the Hurricane system.
     *
     * @param string $name The name of the server group.
     *
     * @return void
     */
    public function registerServer($name)
    {
        $this->send(
            new \Erlang\Tuple(array(
                new \Erlang\Atom('register_with_group'),
                new \Erlang\Atom($name)
            ))
        );
    }

    /**
     * When running over Standard I/O, indicate that the process has
     * successfully started up and is ready to receive requests.
     *
     * @return void
     */
    public function sendReadySignal($name)
    {
        $this->send(new \Erlang\Tuple(array(new Erlang\Atom('ready'))));
    }

    /**
     * Receive the next Hurricane message, turn it into a Message
     * object.
     *
     * @return Message The message that was received.
     */
    public function recv()
    {
        $data = parent::recv();
        $message = new Message();
        $message->setType($data->data[0]);
        $message->setDestination($data->data[1]);
        $message->setTag($data->data[2]);
        $message->setData($data->data[3]);
        return $message;
    }

    /**
     * Turn a message object into a Hurricane message and send it. Can
     * also send other message types if needed.
     *
     * @param mixed $message The data to send.
     *
     * @return Message The message that was received.
     */
    public function send($message)
    {
        if ($message instanceof Message) {
            $destination = $message->getDestination();
            if (is_string($destination)) {
                $destination = new \Erlang\Atom($destination);
            }

            $data = new \Erlang\Tuple(array(
                new \Erlang\Atom($message->getType()),
                $destination,
                $message->getTag(),
                $message->getData(),
                $message->getTimeout()
            ));
        } else {
            $data = $message;
        }

        parent::send($data);
    }
}
