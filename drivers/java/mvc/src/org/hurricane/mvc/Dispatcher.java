package org.hurricane.mvc;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;


public class Dispatcher {
    private static Response handleException(Request request, Throwable exception) {
        ErrorController controller = new ErrorController(request, exception);
        controller.index();
        return controller.getResponse();
    }

    public static Response dispatch(Request request, Class<?> clazz,
            String method) {
        try {
            Object controller = null;
            Constructor<?>[] constructors = clazz.getConstructors();
            for (Integer i = 0; i < constructors.length; i++) {
                Class<?>[] ctorTypes = constructors[i].getParameterTypes();
                if (ctorTypes.length == 1
                        && ctorTypes[0].getCanonicalName().equals(
                                "com.sportsmagik.mvc.Request")) {
                    controller = constructors[i].newInstance(request);
                }
            }

            Method[] methods = clazz.getDeclaredMethods();
            Boolean methodInvoked = false;
            for (Integer i = 0; i < methods.length; i++) {
                if (methods[i].getName().equals(method)) {
                    methods[i].invoke(controller);
                    methodInvoked = true;
                }
            }
            if (!methodInvoked) {
                throw new IllegalStateException(
                        "No method for the request was invoked!");
            }

            return ((Controller) controller).getResponse();
        } catch (InvocationTargetException exception) {
            Throwable filled = exception.fillInStackTrace();
            return handleException(request, filled);
        } catch (Exception exception) {
            return handleException(request, exception);
        }
    }
}
