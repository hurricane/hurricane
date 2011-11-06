package org.hurricane.driver.datatypes;

import java.util.ArrayList;
import java.util.List;

/**
 * Implements a tuple object to be used with Erlang messaging.
 */
public class Tuple {
    /**
     * The list of elements contains in this Tuple.
     */
    private List<Object> mElements = new ArrayList<Object>();

    /**
     * Construct an empty tuple.
     */
    public Tuple() {
    }

    /**
     * Construct a tuple based on the initial things given.
     * 
     * @param init
     */
    public Tuple(Object... init) {
        for (Integer i = 0; i < init.length; i++) {
            mElements.add(init[i]);
        }
    }

    /**
     * Construct a tuple with an initial capacity. Useful when the size is known
     * beforehand (for a Tuple, this should be the case almost always).
     * 
     * @param capacity
     */
    public Tuple(Integer capacity) {
        mElements = new ArrayList<Object>(capacity);
    }

    /**
     * Return a reference to the tuple elements list.
     * 
     * @return A reference to the Tuple elements list.
     */
    public List<Object> elements() {
        return mElements;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(");
        for (int i = 0; i < mElements.size(); i++) {
            builder.append(mElements.get(i));
            if (i + 1 < mElements.size()) {
                builder.append(", ");
            }
        }
        builder.append(")");
        return builder.toString();
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Tuple) {
            Tuple ot = (Tuple) other;
            Boolean equal = mElements.size() == ot.mElements.size();

            if (!equal) {
                return false;
            }

            for (int i = 0; i < mElements.size(); i++) {
                equal = equal && mElements.get(i).equals(ot.mElements.get(i));
            }

            return equal;
        }
        return false;
    }
}
