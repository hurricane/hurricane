package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang pid.
 */
public class Pid {
    /**
     * The atomic name of this pid.
     */
    private Atom mAtom;

    /**
     * The BEAM identifier of this pid.
     */
    private Integer mIdentifier;

    /**
     * The serial number of this pid.
     */
    private Integer mSerial;

    /**
     * The creation sequence number of this pid.
     */
    private Byte mCreation;

    /**
     * Set the given data on the object.
     * 
     * @param atom
     * @param identifier
     * @param serial
     * @param creation
     */
    public Pid(Atom atom, Integer identifier, Integer serial, Byte creation) {
        mAtom = atom;
        mIdentifier = identifier;
        mSerial = serial;
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
     * Getter for serial.
     * 
     * @return the serial.
     */
    public Integer getSerial() {
        return mSerial;
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
        return mAtom + ":<" + mSerial + "." + mIdentifier + "." + mCreation
                + ">";
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
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
