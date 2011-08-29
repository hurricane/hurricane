package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang reference.
 */
public class Reference {
    /**
     * The atomic name of this reference.
     */
    public Atom mAtom;

    /**
     * The BEAM identifier of this reference.
     */
    public Integer mIdentifier;

    /**
     * The creation sequence number of this reference.
     */
    public Byte mCreation;

    /**
     * Set the given data on the object.
     * 
     * @param atom
     * @param identifier
     * @param creation
     */
    public Reference(Atom atom, Integer identifier, Byte creation) {
        mAtom = atom;
        mIdentifier = identifier;
        mCreation = creation;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return mAtom + "#Ref<" + mCreation + ", " + mIdentifier + ">";
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Reference) {
            Reference or = (Reference) other;
            Boolean equal = true;
            equal = equal && mAtom.equals(or.mAtom);
            equal = equal && mIdentifier.equals(or.mIdentifier);
            equal = equal && mCreation.equals(or.mCreation);
            return equal;
        }
        return false;
    }
}
