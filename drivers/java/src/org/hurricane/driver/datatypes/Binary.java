package org.hurricane.driver.datatypes;

import org.hurricane.driver.Utils;

public class Binary extends Base {
    public byte[] mData;

    public Binary(byte[] data) {
        mData = data;
    }

    public String toString() {
        return "<<" + Utils.bytesToString(mData) + ">>";
    }

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
