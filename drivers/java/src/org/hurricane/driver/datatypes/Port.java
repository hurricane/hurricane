package org.hurricane.driver.datatypes;

public class Port extends Base {
    public Atom mAtom;
    public Integer mIdentifier;
    public Byte mCreation;

    public Port(Atom atom, Integer identifier, Byte creation) {
        mAtom = atom;
        mIdentifier = identifier;
        mCreation = creation;
    }

    public String toString() {
        return "Port(" + mAtom + ", " + mCreation + ", " + mIdentifier + ")";
    }

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
