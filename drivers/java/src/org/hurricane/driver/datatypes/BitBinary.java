package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang bit binary.
 */
public class BitBinary {
    /**
     * The number of bits used for the last char.
     */
    private Byte mBits;

    /**
     * The raw bytes used by this bit binary.
     */
    private String mData;

    /**
     * Set the given data on the object.
     * 
     * @param bits
     * @param data
     */
    public BitBinary(byte bits, byte[] data) {
        mBits = (Byte) bits;
        mData = new String(data);
    }

    /**
     * Getter for the bits.
     * 
     * @return the bits.
     */
    public Byte getBits() {
        return mBits;
    }

    /**
     * Getter for the data.
     * 
     * @return the data.
     */
    public String getData() {
        return mData;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        StringBuilder builder = new StringBuilder();
        byte[] data = mData.getBytes();
        builder.append("<<");
        for (Integer i = 0; i < data.length - 1; i++) {
            builder.append(((Byte) data[i]).toString());
            builder.append(", ");
        }
        builder.append(((Byte) data[data.length - 1]).toString());
        builder.append(":");
        builder.append(mBits.toString());
        builder.append(">>");
        return builder.toString();
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof BitBinary) {
            BitBinary ob = (BitBinary) other;
            Boolean equal = true;
            equal = equal && mBits.equals(ob.mBits);
            equal = equal && mData.equals(ob.mData);
            return equal;
        }
        return false;
    }
}
