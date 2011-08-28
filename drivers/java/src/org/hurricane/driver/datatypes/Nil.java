package org.hurricane.driver.datatypes;

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
