package org.hurricane.driver;

import java.io.IOException;

/**
 * Implements a class that can be used to conveniently interface with Hurricane
 * to send/receive messages.
 */
public class Gateway {
    /**
     * The raw stream device object.
     */
    private StreamInterface mStream = null;

    /**
     * The stream emulator object.
     */
    private StreamEmulator mStreamWrapper = null;

    /**
     * Initialize a gateway and interface with Standard I/O.
     * 
     * @throws IOException
     */
    public Gateway() throws IOException {
        setStream(new StdioWrapper());
        mStreamWrapper = new StreamEmulator();
    }

    /**
     * Initialize a gateway for a custom stream device object.
     * 
     * @param stream
     * @throws IOException
     */
    public Gateway(StreamInterface stream) throws IOException {
        setStream(stream);
        mStreamWrapper = new StreamEmulator();
    }

    /**
     * Initialize a gateway for TCP transport using the given host and port.
     * 
     * @param host
     * @param port
     * @throws IOException
     */
    public Gateway(String host, Integer port) throws IOException {
        mStream = new SocketWrapper(host, port);
        mStreamWrapper = new StreamEmulator();
    }

    /**
     * Sets a new stream device object on this gateway.
     * 
     * @param stream
     */
    public void setStream(StreamInterface stream) throws IOException {
        close();
        mStream = stream;
    }

    /**
     * Close the stream device.
     * 
     * @throws IOException
     */
    public void close() throws IOException {
        if (mStream != null) {
            mStream.close();
        }
    }

    /**
     * Receive a single message from Hurricane.
     * 
     * @return the received message
     * @throws RuntimeException
     * @throws IOException
     */
    public Object recv() throws RuntimeException, IOException {
        byte[] messageLenBytes = mStream.read(4);
        if (messageLenBytes.length < 4) {
            throw new RuntimeException(
                    "Message size payload should be 4 bytes!");
        }

        Integer messageLen = Utils.unpackNumber(messageLenBytes).intValue();
        mStreamWrapper.clear();
        mStreamWrapper.write(mStream.read(messageLen));
        Object message = Decoder.decode(mStreamWrapper);
        return message;
    }

    /**
     * Send a single message to Hurricane.
     * 
     * @param message
     * @throws IOException
     */
    public void send(Object message) throws IOException {
        mStreamWrapper.clear();
        Encoder.encode(message, mStreamWrapper);
        mStream.write(Utils.packNumber(mStreamWrapper.size()));
        mStream.write(mStreamWrapper.getBytes());
        mStream.flush();
    }
}
