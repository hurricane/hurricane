package org.hurricane.driver;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

public class SocketWrapper implements StreamInterface {
    public BufferedInputStream mIn;
    public BufferedOutputStream mOut;
    public Socket mSocket;

    public SocketWrapper(String host, int port) throws IOException,
            UnknownHostException {
        mSocket = new Socket(host, port);
        mIn = new BufferedInputStream(mSocket.getInputStream());
        mOut = new BufferedOutputStream(mSocket.getOutputStream());
    }

    public byte[] read(int bytes) throws IOException {
        byte[] rawBytes = new byte[bytes];
        mIn.read(rawBytes, 0, bytes);
        return rawBytes;
    }

    public void write(byte[] data) throws IOException {
        mOut.write(data, 0, data.length);
    }

    public void flush() throws IOException {
        mOut.flush();
    }

    public void close() throws IOException {
        mSocket.close();
    }
}
