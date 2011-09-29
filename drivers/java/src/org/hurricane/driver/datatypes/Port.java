package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang port.
 */
public class Port {
    /**
     * The atomic name of this port.
     */
    private Atom mAtom;

    /**
     * The BEAM identifier of this port.
     */
    private Integer mIdentifier;

    /**
     * The creation sequence number of this port.
     */
    private Byte mCreation;

    /**
     * Set the given data on the object.
     * 
     * @param atom
     * @param identifier
     * @param creation
     */
    public Port(Atom atom, Integer identifier, Byte creation) {
        mAtom = atom;
        mIdentifier = identifier;
        mCreation = creation;
    }

    /**
     * Getter for atom.
     * 
     * @return the atom.
     */
    public Atom getAtom() {
        return mAtom;
    }

    /**
     * Getter for identifier.
     * 
     * @return the identifier.
     */
    public Integer getIdentifier() {
        return mIdentifier;
    }

    /**
     * Getter for creation.
     * 
     * @return the creation.
     */
    public Byte getCreation() {
        return mCreation;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return "Port(" + mAtom + ", " + mCreation + ", " + mIdentifier + ")";
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Port) {
            Port op = (Port) other;
            Boolean equal = true;
            equal = equal && mAtom.equals(op.mAtom);
            equal = equal && mIdentifier.equals(op.mIdentifier);
            equal = equal && mCreation.equals(op.mCreation);
            return equal;
        }
        return false;
    }
}
