package org.hurricane.driver;

import java.util.ArrayList;
import org.hurricane.driver.StreamInterface;

public class StreamEmulator implements StreamInterface {
    public ArrayList<Byte> mBuffer;
    public int mPos;

    public StreamEmulator() {
        clear();
    }

    public StreamEmulator(byte[] data) {
        clear(data.length);
        write(data);
    }

    public Integer size() {
        return mBuffer.size();
    }

    public byte[] getBytes() {
        byte[] bytes = new byte[mBuffer.size()];
        for (Integer i = 0; i < mBuffer.size(); i++) {
            bytes[i] = mBuffer.get(i);
        }
        return bytes;
    }

    public byte[] read(int bytes) {
        if (mBuffer.size() < mPos + bytes) {
            throw new ArrayIndexOutOfBoundsException(
                "Out of data to read (was asked for " + bytes +
                " bytes(s), only " + (mBuffer.size() - mPos) + 
                " bytes(s) remain)");
        }

        byte[] rawBytes = new byte[bytes];
        for (int i = 0; i < bytes; i++) {
            rawBytes[i] = mBuffer.get(mPos);
            mPos++;
        }
        return rawBytes;
    }

    public void write(byte[] data) {
        mBuffer.ensureCapacity(mBuffer.size() + data.length);
        for (int i = 0; i < data.length; i++) {
            mBuffer.add((Byte) data[i]);
        }
    }

    public void flush() {
    }

    public void clear() {
        mBuffer = new ArrayList<Byte>();
        mPos = 0;
    }

    public void clear(int initialCapacity) {
        mBuffer = new ArrayList<Byte>(initialCapacity);
        mPos = 0;
    }

    public void close() {
    }
}
