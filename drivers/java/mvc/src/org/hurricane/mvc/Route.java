package org.hurricane.mvc;

public class Route {
    private String mPattern;
    private Class<?> mController;
    private String mMethod;

    public Route(String pattern, Class<?> controller, String method) {
        setPattern(pattern);
        setController(controller);
        setMethod(method);
    }

    public String getPattern() {
        return mPattern;
    }

    public void setPattern(String pattern) {
        mPattern = pattern;
    }

    public Class<?> getController() {
        return mController;
    }

    public void setController(Class<?> controller) {
        mController = controller;
    }

    public String getMethod() {
        return mMethod;
    }

    public void setMethod(String method) {
        mMethod = method;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("Route={");
        builder.append("pattern=").append(getPattern()).append(", ");
        builder.append("controller=").append(getClass()).append(", ");
        builder.append("method=").append(getMethod());
        builder.append("}");
        return builder.toString();
    }
}
