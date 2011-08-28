package org.hurricane.driver.datatypes;

public class AtomCacheRef extends Base {
    public Byte mValue;

    public AtomCacheRef(byte value) {
        mValue = value;
    }

    public String toString() {
        return "AtomCacheRef: " + mValue;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof AtomCacheRef) {
            if (mValue.equals(((AtomCacheRef) other).mValue)) {
                return true;
            }
        }
        return false;
    }
}
