package org.hurricane.driver.datatypes;

import org.hurricane.driver.datatypes.Base;
import org.hurricane.driver.datatypes.Atom;

public class Pid extends Base {
    public Atom mAtom;
    public Integer mIdentifier;
    public Integer mSerial;
    public Byte mCreation;

    public Pid(Atom atom, Integer identifier, Integer serial, Byte creation) {
        mAtom = atom;
        mIdentifier = identifier;
        mSerial = serial;
        mCreation = creation;
    }

    public String toString() {
        return mAtom + ":<" + mSerial + "." + mIdentifier + "." + mCreation + ">";
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Pid) {
            Pid op = (Pid) other;
            Boolean equal = true;
            equal = equal && mAtom.equals(op.mAtom);
            equal = equal && mSerial.equals(op.mSerial);
            equal = equal && mIdentifier.equals(op.mIdentifier);
            equal = equal && mCreation.equals(op.mCreation);
            return equal;
        }
        return false;
    }
}
