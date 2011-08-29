package org.hurricane.driver;

import java.io.IOException;
import java.util.ArrayList;

/**
 * Emulates a stream. Useful for debugging and working with packet/4 TCP
 * protocols.
 */
public class StreamEmulator implements StreamInterface {
    /**
     * The data buffer.
     */
    private ArrayList<Byte> mBuffer = new ArrayList<Byte>();

    /**
     * The position in the data buffer.
     */
    private int mPos;

    /**
     * Construct an empty stream emulator.
     */
    public StreamEmulator() {
    }

    /**
     * Construct a stream emulator with the given data as the initial state.
     * 
     * @param data
     */
    public StreamEmulator(byte[] data) {
        clear(data.length);
        write(data);
    }

    /**
     * Return the size of the data buffer.
     * 
     * @return
     */
    public Integer size() {
        return mBuffer.size();
    }

    /**
     * Get the raw bytes in the data buffer.
     * 
     * @return
     */
    public byte[] getBytes() {
        byte[] bytes = new byte[mBuffer.size()];
        for (Integer i = 0; i < mBuffer.size(); i++) {
            bytes[i] = mBuffer.get(i);
        }
        return bytes;
    }

    /**
     * Read the specified number of bytes.
     * 
     * @param bytes
     * @return the read bytes
     * @throws IOException
     */
    public byte[] read(int bytes) {
        if (mBuffer.size() < mPos + bytes) {
            throw new ArrayIndexOutOfBoundsException(
                    "Out of data to read (was asked for " + bytes
                            + " bytes(s), only " + (mBuffer.size() - mPos)
                            + " bytes(s) remain)");
        }

        byte[] rawBytes = new byte[bytes];
        for (int i = 0; i < bytes; i++) {
            rawBytes[i] = mBuffer.get(mPos);
            mPos++;
        }
        return rawBytes;
    }

    /**
     * Write the given data.
     * 
     * @param data
     * @throws IOException
     */
    public void write(byte[] data) {
        mBuffer.ensureCapacity(mBuffer.size() + data.length);
        for (int i = 0; i < data.length; i++) {
            mBuffer.add((Byte) data[i]);
        }
    }

    /**
     * Exist for interface completeness.
     */
    public void flush() {
    }

    /**
     * Reset the data buffer and data buffer position to the initial state.
     */
    public void clear() {
        mBuffer.clear();
        mPos = 0;
    }

    /**
     * Reset the data buffer and data buffer position to the initial state,
     * ensuring the data buffer has an initial specified capacity.
     * 
     * @param initialCapacity
     */
    public void clear(int initialCapacity) {
        clear();
        mBuffer.ensureCapacity(initialCapacity);
    }

    /**
     * Exist for interface completeness.
     */
    public void close() {
    }
}
