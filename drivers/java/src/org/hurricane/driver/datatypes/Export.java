package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang export.
 */
public class Export {
    /**
     * The module that this export references.
     */
    public Object mModule;

    /**
     * The function that this export references.
     */
    public Object mFunction;

    /**
     * The arity of the function that this export references.
     */
    public Byte mArity;

    /**
     * Set the given data on the object.
     * 
     * @param module
     * @param function
     * @param arity
     */
    public Export(Object module, Object function, Byte arity) {
        mModule = module;
        mFunction = function;
        mArity = arity;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return mModule + ":" + mFunction + "/" + mArity;
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof Export) {
            Export oe = (Export) other;
            Boolean equal = true;
            equal = equal && mModule.equals(oe.mModule);
            equal = equal && mFunction.equals(oe.mFunction);
            equal = equal && mArity.equals(oe.mArity);
            return equal;
        }
        return false;
    }
}
