package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang atom.
 */
public class Atom {
    /**
     * The name of the atom.
     */
    private String mName;

    /**
     * Set the given data on the object.
     * 
     * @param name
     */
    public Atom(String name) {
        mName = name;
    }
    
    /**
     * Getter for the name.
     * 
     * @return the name.
     */
    public String getName() {
        return mName;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return "Atom(" + mName + ")";
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Atom) {
            if (mName.equals(((Atom) other).mName)) {
                return true;
            }
        }
        return false;
    }
}
