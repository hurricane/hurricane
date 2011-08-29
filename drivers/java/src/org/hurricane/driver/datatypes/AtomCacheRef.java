package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang atom cache ref.
 */
public class AtomCacheRef {
    /**
     * The value of the atom cache ref.
     */
    public Byte mValue;

    /**
     * Set the given data on the object.
     * 
     * @param value
     */
    public AtomCacheRef(byte value) {
        mValue = value;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return "AtomCacheRef: " + mValue;
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
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
