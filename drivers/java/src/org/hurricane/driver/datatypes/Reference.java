package org.hurricane.driver.datatypes;

import org.hurricane.driver.datatypes.Base;
import org.hurricane.driver.datatypes.Atom;

public class Reference extends Base {
    public Atom mAtom;
    public Integer mIdentifier;
    public Byte mCreation;

    public Reference(Atom atom, Integer identifier, Byte creation) {
        mAtom = atom;
        mIdentifier = identifier;
        mCreation = creation;
    }

    public String toString() {
        return mAtom + "#Ref<" + mCreation + ", " + mIdentifier + ">";
    }

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
