package org.hurricane.mvc;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import net.asfun.jangod.template.TemplateEngine;

public class Controller {
    private Request mRequest;
    private Response mResponse;
    private Map<String, Object> mViewData;
    private static final TemplateEngine TEMPLATE_ENGINE = new TemplateEngine();
    static {
        setViewDir(".");
    }

    public Controller(Request request) {
        setRequest(request);
        setResponse(new Response());
        setViewData(new HashMap<String, Object>());
    }

    public static void setViewDir(String rootPath) {
        TEMPLATE_ENGINE.getConfiguration().setWorkspace(rootPath);
    }

    public Request getRequest() {
        return mRequest;
    }

    public void setRequest(Request request) {
        mRequest = request;
    }

    public Response getResponse() {
        return mResponse;
    }

    public void setResponse(Response response) {
        mResponse = response;
    }

    public Map<String, Object> getViewData() {
        return mViewData;
    }

    public void setViewData(Map<String, Object> viewData) {
        mViewData = viewData;
    }

    public void setViewVar(String name, Object value) {
        mViewData.put(name, value);
    }

    public Object getViewVar(String name) {
        return mViewData.get(name);
    }

    public Boolean viewVarExists(String name) {
        return mViewData.containsKey(name);
    }

    public String render(String templateFile) throws IOException {
        return realRender(getClass().getSimpleName().replace("Controller", "")
                .toLowerCase()
                + "/" + templateFile + ".tpl");
    }

    public String realRender(String templateFile) throws IOException {
        return TEMPLATE_ENGINE.process(templateFile, getViewData());
    }
}
