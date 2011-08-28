package org.hurricane.driver.datatypes;

import java.util.ArrayList;

public class NewReference extends Base {
    public Object mAtom;
    public Byte mCreation;
    public ArrayList<Integer> mIds;

    public NewReference(Object atom, Byte creation, ArrayList<Integer> ids) {
        mAtom = atom;
        mCreation = creation;
        mIds = ids;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(mAtom);
        builder.append("#Ref<");
        builder.append(mCreation);
        for (Integer i = 0; i < mIds.size(); i++) {
            builder.append(".");
            builder.append(mIds.get(i));
        }
        builder.append(">");
        return builder.toString();
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof NewReference) {
            NewReference on = (NewReference) other;
            Boolean equal = true;
            equal = equal && mAtom.equals(on.mAtom);
            equal = equal && mCreation.equals(on.mCreation);

            for (Integer i = 0; i < mIds.size(); i++) {
                equal = equal && mIds.get(i).equals(on.mIds.get(i));
            }
            return equal;
        }
        return false;
    }
}
