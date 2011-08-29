package org.hurricane.driver;

import java.io.IOException;

/**
 * Specifies the interface that must be implemented for anything to be a Gateway
 * transport driver.
 */
public interface StreamInterface {
    /**
     * Read the specified number of bytes.
     * 
     * @param bytes
     * @return the read bytes
     * @throws IOException
     */
    public byte[] read(int bytes) throws IOException;

    /**
     * Write the given data.
     * 
     * @param data
     * @throws IOException
     */
    public void write(byte[] data) throws IOException;

    /**
     * Send all buffered data unconditionally over the device.
     * 
     * @throws IOException
     */
    public void flush() throws IOException;

    /**
     * Close the input/ouput device(s).
     * 
     * @throws IOException
     */
    public void close() throws IOException;
}
