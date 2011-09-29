package org.hurricane.driver.datatypes;

/**
 * Implements an Erlang export.
 */
public class Export {
    /**
     * The module that this export references.
     */
    private Object mModule;

    /**
     * The function that this export references.
     */
    private Object mFunction;

    /**
     * The arity of the function that this export references.
     */
    private Byte mArity;

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
     * Getter for module.
     * 
     * @return the module.
     */
    public Object getModule() {
        return mModule;
    }
    
    /**
     * Getter for function.
     * 
     * @return the function.
     */
    public Object getFunction() {
        return mFunction; 
    }
    
    /**
     * Getter for arity.
     * 
     * @return the arity.
     */
    public Byte getArity() {
        return mArity;
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
