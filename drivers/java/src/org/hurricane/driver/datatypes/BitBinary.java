package org.hurricane.driver.datatypes;

public class BitBinary extends Base {
    public Byte mBits;
    public String mData;

    public BitBinary(byte bits, byte[] data) {
        mBits = (Byte) bits;
        mData = new String(data);
    }

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
