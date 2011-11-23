package org.hurricane.mvc;



public class NotFoundController extends Controller {
    public NotFoundController(Request request) {
        super(request);
    }

    public void index() {
        getResponse().setCode(404);
        getResponse().setBody("404 -- Not Found");
        getResponse().addHeader("Content-Type", "text/html");
    }
}
