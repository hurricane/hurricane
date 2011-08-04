package org.hurricane.driver;

import java.io.IOException;
import org.hurricane.driver.Encoder;
import org.hurricane.driver.Decoder;
import org.hurricane.driver.StreamInterface;
import org.hurricane.driver.StdioWrapper;
import org.hurricane.driver.SocketWrapper;
import org.hurricane.driver.StreamEmulator;
import org.hurricane.driver.Utils;

public class Gateway {
    private StreamInterface mStream;
    private StreamEmulator mStreamWrapper;

    public Gateway() throws IOException {
        mStream = new StdioWrapper();
        mStreamWrapper = new StreamEmulator();
    }

    public Gateway(String host, Integer port) throws IOException {
        mStream = new SocketWrapper(host, port);
        mStreamWrapper = new StreamEmulator();
    }

    public void setStream(StreamInterface stream) {
        mStream = stream;
    }

    public void close() throws IOException {
        mStream.close();
    }

    public Object recv() throws RuntimeException, IOException {
        byte[] messageLenBytes = mStream.read(4);
        if (messageLenBytes.length < 4) {
            throw new RuntimeException("Message size payload should be 4 bytes!");
        }

        Integer messageLen = Utils.unpackNumber(messageLenBytes).intValue();
        mStreamWrapper.clear();
        mStreamWrapper.write(mStream.read(messageLen));
        Object message = Decoder.decode(mStreamWrapper);
        return message;
    }

    public void send(Object message) throws IOException {
        mStreamWrapper.clear();
        Encoder.encode(message, mStreamWrapper);
        mStream.write(Utils.packNumber(mStreamWrapper.size()));
        mStream.write(mStreamWrapper.getBytes());
        mStream.flush();
    }
}
