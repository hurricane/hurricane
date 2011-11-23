package org.hurricane.mvc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.Nil;
import org.hurricane.driver.datatypes.Tuple;

public class Request extends HttpObject {
    private String mScheme;
    private Float mVersion;
    private String mMethod;
    private String mPath;
    private Map<String, String> mParams;
    private String mBody;

    public Request(List<Object> propList) {
        super();

        Iterator<Object> propIter = propList.iterator();
        while (propIter.hasNext()) {
            Tuple pair = (Tuple) propIter.next();
            Atom key = (Atom) pair.elements().get(0);
            Object value = pair.elements().get(1);
            if (key.getName().equals("scheme")) {
                setScheme(((Atom) value).getName());
            } else if (key.getName().equals("version")) {
                Tuple versionTuple = (Tuple) value;
                setVersion(((Integer) versionTuple.elements().get(0))
                        .floatValue()
                        + ((Integer) versionTuple.elements().get(1)) * 0.1f);
            } else if (key.getName().equals("method")) {
                setMethod(((Atom) value).getName());
            } else if (key.getName().equals("headers")) {
                @SuppressWarnings("unchecked")
                Iterator<Object> headerIter = ((List<Object>) value).iterator();
                while (headerIter.hasNext()) {
                    Tuple header = (Tuple) headerIter.next();
                    Atom headerName = (Atom) header.elements().get(0);
                    String headerValue = (String) header.elements().get(1);
                    mHeaders.put(headerName.getName(), headerValue);
                }
            } else if (key.getName().equals("path")) {
                setPath((String) value);
            } else if (key.getName().equals("params")) {
                if (value instanceof Nil) {
                    continue;
                }

                mParams = new HashMap<String, String>();
                @SuppressWarnings("unchecked")
                Iterator<Object> paramIter = ((List<Object>) value).iterator();
                while (paramIter.hasNext()) {
                    Tuple param = (Tuple) paramIter.next();
                    String paramName = (String) param.elements().get(0);
                    String paramValue = (String) param.elements().get(1);
                    mParams.put(paramName, paramValue);
                }
            } else if (key.getName().equals("body")) {
                if (value instanceof Atom) {
                    setBody(null);
                } else {
                    setBody((String) value);
                }
            }
        }
    }

    public String getScheme() {
        return mScheme;
    }

    public void setScheme(String scheme) {
        mScheme = scheme;
    }

    public Float getVersion() {
        return mVersion;
    }

    public void setVersion(Float version) {
        mVersion = version;
    }

    public String getMethod() {
        return mMethod;
    }

    public void setMethod(String method) {
        mMethod = method;
    }

    public String getPath() {
        return mPath;
    }

    public void setPath(String path) {
        mPath = path;
    }

    public Map<String, String> getParams() {
        return mParams;
    }

    public String getParam(String name) {
        return getParams().get(name);
    }

    public String getParam(String name, String defaultValue) {
        String value = getParam(name);
        if (value == null) {
            return defaultValue;
        }
        return value;
    }

    public void setParams(Map<String, String> params) {
        mParams = params;
    }

    public String getBody() {
        return mBody;
    }

    public void setBody(String body) {
        mBody = body;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("Request={");
        builder.append("scheme=").append(getScheme()).append(", ");
        builder.append("version=").append(getVersion()).append(", ");
        builder.append("method=").append(getMethod()).append(", ");
        builder.append("path=").append(getPath()).append(", ");
        builder.append("params=").append(getParams()).append(", ");
        builder.append("headers=").append(getHeaders()).append(", ");
        builder.append("body=").append(getBody());
        builder.append("}");
        return builder.toString();
    }
}
