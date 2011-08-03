package org.hurricane.driver;

import java.io.IOException;

public interface StreamInterface {
    public byte[] read(int bytes) throws IOException;

    public void write(byte[] data) throws IOException;

    public void flush() throws IOException;

    public void close() throws IOException;
}
