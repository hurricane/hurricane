package org.hurricane.driver.datatypes;

import org.hurricane.driver.datatypes.Base;

public class Binary extends Base {
    public String mData;

    public Binary(byte[] data) {
        mData = new String(data);
    }

    public String toString() {
        return "<<" + mData + ">>";
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Binary) {
            Binary ob = (Binary) other;
            if (mData.equals(ob.mData)) {
                return true;
            }
        }
        return false;
    }
}
