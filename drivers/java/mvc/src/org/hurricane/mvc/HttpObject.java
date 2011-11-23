package org.hurricane.mvc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hurricane.driver.datatypes.Tuple;

public abstract class HttpObject {
    protected Map<String, String> mHeaders;

    public HttpObject() {
        mHeaders = new HashMap<String, String>();
    }

    public Map<String, String> getHeaders() {
        return mHeaders;
    }

    public List<Tuple> getHeadersList() {
        List<Tuple> headersList = new ArrayList<Tuple>();
        Iterator<String> headerIter = mHeaders.keySet().iterator();
        while (headerIter.hasNext()) {
            String name = headerIter.next();
            Tuple header = new Tuple();
            header.elements().add(name);
            header.elements().add(mHeaders.get(name));
            headersList.add(header);
        }
        return headersList;
    }

    public void setHeaders(Map<String, String> headers) {
        mHeaders = headers;
    }

    public void addHeader(String name, String value) {
        mHeaders.put(name, value);
    }
}
