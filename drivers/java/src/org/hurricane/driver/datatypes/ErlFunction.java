package org.hurricane.driver.datatypes;

import java.util.ArrayList;
import org.hurricane.driver.datatypes.Base;
import org.hurricane.driver.datatypes.Pid;

public class ErlFunction extends Base {
    public Pid mPid;
    public Object mModule;
    public Object mIndex;
    public Object mUniq;
    public ArrayList<Object> mFreeVars;

    public ErlFunction(Pid pid, Object module, Object index, Object uniq, ArrayList<Object> freeVars) {
        mPid = pid;
        mModule = module;
        mIndex = index;
        mUniq = uniq;
        mFreeVars = freeVars;
    }
    
    public String toString() {
        return mModule + ":" + mUniq;
    }

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
