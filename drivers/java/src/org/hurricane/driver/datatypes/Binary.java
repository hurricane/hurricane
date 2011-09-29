package org.hurricane.driver.datatypes;

import org.hurricane.driver.Utils;

/**
 * Implements an Erlang binary.
 */
public class Binary {
    /**
     * The raw data stored in this binary.
     */
    private byte[] mData;

    /**
     * Set the given data on the object.
     * 
     * @param data
     */
    public Binary(byte[] data) {
        mData = data;
    }
    
    /**
     * Getter for the data.
     * 
     * @return the data.
     */
    public byte[] getData() {
        return mData;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return "<<" + Utils.bytesToString(mData) + ">>";
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Binary) {
            Binary ob = (Binary) other;

            if (mData.length != ob.mData.length) {
                return false;
            }

            for (Integer i = 0; i < mData.length; i++) {
                if (mData[i] != ob.mData[i]) {
                    return false;
                }
            }

            return true;
        }
        return false;
    }
}
