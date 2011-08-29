package org.hurricane.driver.datatypes;

/**
 * Implements the Erlang nil.
 */
public class Nil {
    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return "Nil";
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Nil) {
            return true;
        }
        return false;
    }
}
