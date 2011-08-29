package org.hurricane.driver.datatypes;

public class Export {
    public Object mModule;
    public Object mFunction;
    public Byte mArity;

    public Export(Object module, Object function, Byte arity) {
        mModule = module;
        mFunction = function;
        mArity = arity;
    }

    public String toString() {
        return mModule + ":" + mFunction + "/" + mArity;
    }

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
