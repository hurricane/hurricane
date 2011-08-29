package org.hurricane.driver.datatypes;

/**
 * 
 */
public class Atom {
    public String mName;

    public Atom(String name) {
        mName = name;
    }

    public String toString() {
        return "Atom(" + mName + ")";
    }

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
