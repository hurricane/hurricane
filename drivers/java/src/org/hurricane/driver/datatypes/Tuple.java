package org.hurricane.driver.datatypes;

import java.util.ArrayList;
import java.util.List;

public class Tuple {
    public ArrayList<Object> mElements;

    public Tuple() {
        mElements = new ArrayList<Object>();
    }

    public Tuple(Integer capacity) {
        mElements = new ArrayList<Object>(capacity);
    }

    public List<Object> elements() {
        return mElements;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(");
        for (int i = 0; i < mElements.size(); i++) {
            builder.append(mElements.get(i).toString());
            builder.append(", ");
        }
        builder.append(")");
        return builder.toString();
    }

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
