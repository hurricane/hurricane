package org.hurricane.driver.datatypes;

import org.hurricane.driver.datatypes.Base;

public class Nil extends Base {
    public String toString() {
        return "Nil";
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Nil) {
            return true;
        }
        return false;
    }
}
