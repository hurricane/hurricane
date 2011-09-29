package org.hurricane.driver.datatypes;

import java.util.List;

/**
 * Implements an Erlang function (defined at compile-time).
 */
public class ErlFunction {
    /**
     * The pid which owns this function.
     */
    private Pid mPid;

    /**
     * The module where this function lives.
     */
    private Object mModule;

    /**
     * The BEAM index of this function.
     */
    private Object mIndex;

    /**
     * The unique BEAM identifier for this function.
     */
    private Object mUniq;

    /**
     * The nested structure of this new function.
     */
    private List<Object> mFreeVars;

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
            List<Object> freeVars) {
        mPid = pid;
        mModule = module;
        mIndex = index;
        mUniq = uniq;
        mFreeVars = freeVars;
    }

    /**
     * Getter for pid.
     * 
     * @return the pid.
     */
    public Pid getPid() {
        return mPid;
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
     * Getter for index.
     * 
     * @return the index.
     */
    public Object getIndex() {
        return mIndex;
    }

    /**
     * Getter for uniq.
     * 
     * @return the uniq.
     */
    public Object getUniq() {
        return mUniq;
    }

    /**
     * Getter for free vars.
     * 
     * @return the free vars.
     */
    public List<Object> getFreeVars() {
        return mFreeVars;
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
