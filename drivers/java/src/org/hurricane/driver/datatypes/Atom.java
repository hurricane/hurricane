package org.hurricane.driver.datatypes;

import org.hurricane.driver.datatypes.Base;

public class Atom extends Base {
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
