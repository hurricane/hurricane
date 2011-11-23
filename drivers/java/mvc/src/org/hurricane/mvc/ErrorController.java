package org.hurricane.mvc;


public class ErrorController extends Controller {
    private Throwable mException;
    private static Boolean SHOW_STACK_TRACES = false;
    private static final String NEWLINE = "<br/>\n";
    private static final String INDENT = "    ";

    public static void setShowStackTraces(Boolean showStackTraces) {
        SHOW_STACK_TRACES = showStackTraces;
    }

    public ErrorController(Request request, Throwable exception) {
        super(request);
        setException(exception);
    }

    public Throwable getException() {
        return mException;
    }

    public void setException(Throwable exception) {
        mException = exception;
    }

    private String stackTraceToString(Throwable exception) {
        StringBuilder builder = new StringBuilder();
        StackTraceElement[] frames = getException().getStackTrace();
        for (Integer i = 0; i < frames.length; i++) {
            builder.append(INDENT).append(frames[i].toString()).append(NEWLINE);
        }
        return builder.toString();
    }

    public void index() {
        getResponse().setCode(500);
        getResponse().addHeader("Content-Type", "text/html");
        getResponse().appendToBody("500 -- Internal Server Error:");
        getResponse().appendToBody(NEWLINE);
        getResponse().appendToBody(getException().getMessage());
        getResponse().appendToBody(NEWLINE).appendToBody(NEWLINE);
        if (SHOW_STACK_TRACES) {
            Boolean firstCause = true;
            Throwable current = getException();
            while (current != null) {
                if (!firstCause) {
                    getResponse().appendToBody("Caused by: ");
                }
                getResponse().appendToBody(current.toString()).appendToBody(
                        NEWLINE);

                if (firstCause) {
                    firstCause = false;
                }
                getResponse().appendToBody(
                        stackTraceToString(current).replace(" ", "&nbsp;"));
                current = current.getCause();
            }
        }

        System.err.println(getException().getMessage());
        getException().printStackTrace();
        System.err.println();
    }
}
