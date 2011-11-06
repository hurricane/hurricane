package org.hurricane.driver;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * Implements the TCP transport logic.
 */
public class SocketWrapper implements StreamInterface {
    /**
     * Buffers the socket input stream for performance.
     */
    private BufferedInputStream mIn;

    /**
     * Buffers the socket output stream for performance.
     */
    private BufferedOutputStream mOut;

    /**
     * The socket device.
     */
    private Socket mSocket;

    /**
     * Open a socket to the given host and port.
     * 
     * @param host
     * @param port
     * @throws IOException
     * @throws UnknownHostException
     */
    public SocketWrapper(String host, int port) throws IOException,
            UnknownHostException {
        mSocket = new Socket(host, port);
        mIn = new BufferedInputStream(mSocket.getInputStream());
        mOut = new BufferedOutputStream(mSocket.getOutputStream());
    }

    /**
     * Read the specified number of bytes.
     * 
     * @param bytes
     * @return the read bytes
     * @throws IOException
     */
    public byte[] read(int bytes) throws IOException {
        Integer offset = 0;
        byte[] rawBytes = new byte[bytes];
        while (offset < bytes) {
            offset += mIn.read(rawBytes, offset, bytes - offset);
        }
        return rawBytes;
    }

    /**
     * Write the given data.
     * 
     * @param data
     * @throws IOException
     */
    public void write(byte[] data) throws IOException {
        mOut.write(data, 0, data.length);
    }

    /**
     * Send all buffered data unconditionally over the device.
     * 
     * @throws IOException
     */
    public void flush() throws IOException {
        mOut.flush();
    }

    /**
     * Close the socket.
     * 
     * @throws IOException
     */
    public void close() throws IOException {
        mSocket.close();
    }
}
