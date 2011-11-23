package org.hurricane.mvc;

import java.util.ArrayList;
import java.util.List;


public class Router {
    private List<Route> mRoutes = new ArrayList<Route>();
    private static Route NOT_FOUND_ROUTE = new Route(null,
            org.hurricane.mvc.NotFoundController.class, "index");

    public Router() {
    }

    public Router(List<Route> routes) {
        setRoutes(routes);
    }

    public List<Route> getRoutes() {
        return mRoutes;
    }

    public void setRoutes(List<Route> routes) {
        mRoutes = routes;
    }

    public void addRoute(Route route) {
        mRoutes.add(route);
    }

    public static void setNotFoundRoute(Route route) {
        NOT_FOUND_ROUTE = route;
    }

    public Route route(Request request) {
        for (Integer i = 0; i < mRoutes.size(); i++) {
            Route route = mRoutes.get(i);
            String pattern = route.getPattern();
            if (request.getPath().matches(pattern)) {
                return route;
            }
        }
        return NOT_FOUND_ROUTE;
    }
}
