package org.hurricane;

import java.io.IOException;

import org.hurricane.driver.StreamInterface;
import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.Tuple;

/**
 * Implements a class that can be used to conveniently interface with Hurricane
 * to send/receive messages.
 */
public class Gateway extends org.hurricane.driver.Gateway {
    /**
     * Initialize a gateway and interface with Standard I/O.
     * 
     * @throws IOException
     */
    public Gateway() throws IOException {
        super();
    }

    /**
     * Initialize a gateway for a custom stream device object.
     * 
     * @param stream
     * @throws IOException
     */
    public Gateway(StreamInterface stream) throws IOException {
        super(stream);
    }

    /**
     * Initialize a gateway for TCP transport using the given host and port.
     * 
     * @param host
     * @param port
     * @throws IOException
     */
    public Gateway(String host, Integer port) throws IOException {
        super(host, port);
    }

    /**
     * When used over Standard I/O, sends the "ready" signal to indicate that
     * the process has successfully started up and is ready to send/receive
     * messages.
     * 
     * @throws IOException
     */
    public void sendReadySignal() throws IOException {
        send(new Tuple(new Atom("ready")));
    }

    /**
     * Registers this process with a named group in Hurricane.
     * 
     * @throws IOException
     */
    public void registerServer(String name) throws IOException {
        send(new Tuple(new Atom("register_with_group"), new Atom(name)));
    }

    /**
     * Receive the next Hurricane message, turn it into a Message object.
     * 
     * @return the received message
     * @throws RuntimeException
     * @throws IOException
     */
    public Message recv() throws RuntimeException, IOException {
        Tuple data = (Tuple) super.recv();
        Atom type = (Atom) data.elements().get(0);

        Message message = new Message();
        message.setType(type.getName());
        message.setDestination(data.elements().get(1));
        message.setTag(data.elements().get(2));
        message.setData(data.elements().get(3));
        return message;
    }

    /**
     * Send a single message to Hurricane.
     * 
     * @param message
     * @throws IOException
     */
    public void send(final Object rawMessage) throws IOException {
        Object data = null;

        if (rawMessage instanceof Message) {
            Message message = (Message) rawMessage;
            Object destination = message.getDestination();
            if (destination instanceof String) {
                destination = new Atom((String) destination);
            }
            data = new Tuple(new Atom(message.getType()), destination,
                    message.getTag(), message.getData(), message.getTimeout());
        } else {
            data = rawMessage;
        }

        super.send(data);
    }
}
