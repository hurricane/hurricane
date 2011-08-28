package org.hurricane.driver;

import java.io.BufferedInputStream;
import java.io.IOException;

public class StdioWrapper implements StreamInterface {
    public BufferedInputStream mIn;

    public StdioWrapper() {
        mIn = new BufferedInputStream(System.in);
    }

    public byte[] read(int bytes) throws IOException {
        byte[] rawBytes = new byte[bytes];
        mIn.read(rawBytes, 0, bytes);
        return rawBytes;
    }

    public void write(byte[] data) {
        System.out.write(data, 0, data.length);
    }

    public void flush() {
        System.out.flush();
    }

    public void close() {
    }
}
