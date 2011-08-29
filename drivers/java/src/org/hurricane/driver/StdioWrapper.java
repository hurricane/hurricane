package org.hurricane.driver;

import java.io.BufferedInputStream;
import java.io.IOException;

/**
 * Implements the Standard I/O transport logic.
 */
public class StdioWrapper implements StreamInterface {
    /**
     * Buffers Standard In for performance.
     */
    private BufferedInputStream mIn;

    /**
     * Initializes all needed streams.
     */
    public StdioWrapper() {
        mIn = new BufferedInputStream(System.in);
    }

    /**
     * Read the specified number of bytes.
     * 
     * @param bytes
     * @return the read bytes
     * @throws IOException
     */
    public byte[] read(int bytes) throws IOException {
        byte[] rawBytes = new byte[bytes];
        mIn.read(rawBytes, 0, bytes);
        return rawBytes;
    }

    /**
     * Write the given data.
     * 
     * @param data
     * @throws IOException
     */
    public void write(byte[] data) {
        System.out.write(data, 0, data.length);
    }

    /**
     * Send all buffered data unconditionally over Standard Out.
     * 
     * @throws IOException
     */
    public void flush() {
        System.out.flush();
    }

    /**
     * Standard I/O should not be closed, so do nothing.
     * 
     * @throws IOException
     */
    public void close() {
    }
}
