package org.hurricane.driver.datatypes;

import java.util.ArrayList;
import org.hurricane.driver.datatypes.Base;
import org.hurricane.driver.datatypes.Pid;

public class NewFunction extends Base {
    public Byte mArity;
    public String mUniq;
    public Integer mIndex;
    public Object mModule;
    public Object mOldIndex;
    public Object mOldUniq;
    public Pid mPid;
    public ArrayList<Object> mFreeVars;

    public NewFunction(Byte arity, String uniq, Integer index, Object module, Object oldIndex, Object oldUniq, Pid pid, ArrayList<Object> freeVars) {
        mArity = arity;
        mUniq = uniq;
        mIndex = index;
        mModule = module;
        mOldIndex = oldIndex;
        mOldUniq = oldUniq;
        mPid = pid;
        mFreeVars = freeVars;
    }

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
    
    public String toString() {
        return mModule + ":" + uniqToHex() + "/" + mArity;
    }

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
