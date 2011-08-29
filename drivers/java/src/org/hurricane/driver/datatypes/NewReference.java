package org.hurricane.driver.datatypes;

import java.util.ArrayList;

/**
 * Implements an Erlang "new reference" (a reference created at runtime).
 */
public class NewReference {
    /**
     * The atomic name of this new reference.
     */
    public Object mAtom;

    /**
     * The creation sequence number of this new reference.
     */
    public Byte mCreation;

    /**
     * The identifiers for this new references.
     */
    public ArrayList<Integer> mIds;

    /**
     * Set the given data on the object.
     * 
     * @param atom
     * @param creation
     * @param ids
     */
    public NewReference(Object atom, Byte creation, ArrayList<Integer> ids) {
        mAtom = atom;
        mCreation = creation;
        mIds = ids;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(mAtom);
        builder.append("#Ref<");
        builder.append(mCreation);
        for (Integer i = 0; i < mIds.size(); i++) {
            builder.append(".");
            builder.append(mIds.get(i));
        }
        builder.append(">");
        return builder.toString();
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof NewReference) {
            NewReference on = (NewReference) other;
            Boolean equal = true;
            equal = equal && mAtom.equals(on.mAtom);
            equal = equal && mCreation.equals(on.mCreation);

            for (Integer i = 0; i < mIds.size(); i++) {
                equal = equal && mIds.get(i).equals(on.mIds.get(i));
            }
            return equal;
        }
        return false;
    }
}
