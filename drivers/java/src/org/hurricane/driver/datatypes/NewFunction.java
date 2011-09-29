package org.hurricane.driver.datatypes;

import java.util.List;

/**
 * Implements an Erlang function (created at run-time, usually with the fun ()
 * -> end syntax).
 */
public class NewFunction {
    /**
     * The arity of this new function.
     */
    private Byte mArity;

    /**
     * The unique BEAM identifier of this new function.
     */
    private String mUniq;

    /**
     * The BEAM index of this new function.
     */
    private Integer mIndex;

    /**
     * The module where this new function originated.
     */
    private Object mModule;

    /**
     * The old BEAM index for this new function (for code swapping).
     */
    private Object mOldIndex;

    /**
     * The old unique BEAM identifier for this new function (for code swapping).
     */
    private Object mOldUniq;

    /**
     * The pid of the process that created this new function.
     */
    private Pid mPid;

    /**
     * The nested structure of this new function.
     */
    private List<Object> mFreeVars;

    /**
     * Set the given data on the object.
     * 
     * @param arity
     * @param uniq
     * @param index
     * @param module
     * @param oldIndex
     * @param oldUniq
     * @param pid
     * @param freeVars
     */
    public NewFunction(Byte arity, String uniq, Integer index, Object module,
            Object oldIndex, Object oldUniq, Pid pid, List<Object> freeVars) {
        mArity = arity;
        mUniq = uniq;
        mIndex = index;
        mModule = module;
        mOldIndex = oldIndex;
        mOldUniq = oldUniq;
        mPid = pid;
        mFreeVars = freeVars;
    }

    /**
     * Return the unique BEAM identifier for this new function as a hexadecimal
     * string.
     * 
     * @return
     */
    public String uniqToHex() {
        StringBuilder builder = new StringBuilder();
        byte[] bytes = mUniq.getBytes();

        String hex;
        for (Integer i = 0; i < bytes.length; i++) {
            hex = Integer.toHexString(0xff & bytes[i]);
            if (hex.length() < 2) {
                builder.append("0");
            }
            builder.append(hex);
        }
        return builder.toString();
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
     * Getter for uniq.
     * 
     * @return the uniq.
     */
    public String getUniq() {
        return mUniq;
    }

    /**
     * Getter for index.
     * 
     * @return the index.
     */
    public Integer getIndex() {
        return mIndex;
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
     * Getter for old index.
     * 
     * @return the old index.
     */
    public Object getOldIndex() {
        return mOldIndex;
    }

    /**
     * Getter for old uniq.
     * 
     * @return the old uniq.
     */
    public Object getOldUniq() {
        return mOldUniq;
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
        return mModule + ":" + uniqToHex() + "/" + mArity;
    }

    /**
     * Compares this to another object for equality.
     * 
     * @param other
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof NewFunction) {
            NewFunction of = (NewFunction) other;
            if (mFreeVars.size() != of.mFreeVars.size()) {
                return false;
            }

            Boolean equal = true;
            equal = equal && mArity.equals(of.mArity);
            equal = equal && mUniq.equals(of.mUniq);
            equal = equal && mIndex.equals(of.mIndex);
            equal = equal && mModule.equals(of.mModule);
            equal = equal && mOldIndex.equals(of.mOldIndex);
            equal = equal && mOldUniq.equals(of.mOldUniq);
            equal = equal && mPid.equals(of.mPid);

            for (Integer i = 0; i < mFreeVars.size(); i++) {
                equal = equal && mFreeVars.get(i).equals(of.mFreeVars.get(i));
            }

            return equal;
        }
        return false;
    }
}
