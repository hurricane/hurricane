package org.hurricane.driver.datatypes;

import java.util.ArrayList;

/**
 * Implements an Erlang function (defined at compile-time).
 */
public class ErlFunction {
    /**
     * The pid which owns this function.
     */
    public Pid mPid;

    /**
     * The module where this function lives.
     */
    public Object mModule;

    /**
     * The BEAM index of this function.
     */
    public Object mIndex;

    /**
     * The unique BEAM identifier for this function.
     */
    public Object mUniq;

    /**
     * The nested structure of this new function.
     */
    public ArrayList<Object> mFreeVars;

    /**
     * Set the given data on the object.
     * 
     * @param pid
     * @param module
     * @param index
     * @param uniq
     * @param freeVars
     */
    public ErlFunction(Pid pid, Object module, Object index, Object uniq,
            ArrayList<Object> freeVars) {
        mPid = pid;
        mModule = module;
        mIndex = index;
        mUniq = uniq;
        mFreeVars = freeVars;
    }

    /**
     * Return a human-readable representation of the object.
     */
    public String toString() {
        return mModule + ":" + mUniq;
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof ErlFunction) {
            ErlFunction of = (ErlFunction) other;
            if (mFreeVars.size() != of.mFreeVars.size()) {
                return false;
            }

            Boolean equal = true;
            equal = equal && mPid.equals(of.mPid);
            equal = equal && mModule.equals(of.mModule);
            equal = equal && mIndex.equals(of.mIndex);
            equal = equal && mUniq.equals(of.mUniq);

            for (Integer i = 0; i < mFreeVars.size(); i++) {
                equal = equal && mFreeVars.get(i).equals(of.mFreeVars.get(i));
            }

            return equal;
        }
        return false;
    }
}
