package org.hurricane.mvc;


public class Response extends HttpObject {
    private Integer mCode;

    private StringBuilder mBody;

    public Response() {
        super();
        mCode = 200;
        mBody = new StringBuilder();
    }

    public Integer getCode() {
        return mCode;
    }

    public void setCode(Integer code) {
        mCode = code;
    }

    public String getBody() {
        return mBody.toString();
    }

    public void setBody(String body) {
        mBody.delete(0, mBody.length());
        mBody.append(body);
    }

    public Response appendToBody(String chunk) {
        mBody.append(chunk);
        return this;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("Response={");
        builder.append("code=").append(getCode()).append(", ");
        builder.append("headers=").append(getHeaders()).append(", ");
        builder.append("body=").append(getBody());
        builder.append("}");
        return builder.toString();
    }
}
