package org.hurricane;

/**
 * Represents a standard Hurricane message.
 */
public class Message {
    /**
     * The type of the message (request, response, etc)
     */
    private String mType = "";

    /**
     * Where this message came from/is going to.
     */
    private Object mDestination = "";

    /**
     * The tag of the message (used for scatter-gather).
     */
    private Object mTag = "";

    /**
     * The data payload of the message.
     */
    private Object mData = "";

    /**
     * The timeout of the message.
     */
    private Integer mTimeout = 10000;

    /**
     * Getter for the message type.
     * 
     * @return
     */
    public String getType() {
        return mType;
    }

    /**
     * Setter for the message type.
     * 
     * @param type
     */
    public void setType(String type) {
        mType = type;
    }

    /**
     * Getter for the message source/destination.
     * 
     * @return
     */
    public Object getDestination() {
        return mDestination;
    }

    /**
     * Setter for the message source/destination.
     * 
     * @param destination
     */
    public void setDestination(Object destination) {
        mDestination = destination;
    }

    /**
     * Getter for the message tag.
     * 
     * @return
     */
    public Object getTag() {
        return mTag;
    }

    /**
     * Setter for the message tag.
     * 
     * @param tag
     */
    public void setTag(Object tag) {
        mTag = tag;
    }

    /**
     * Getter for the message data.
     * 
     * @return
     */
    public Object getData() {
        return mData;
    }

    /**
     * Setter for the message data.
     * 
     * @param data
     */
    public void setData(Object data) {
        mData = data;
    }

    /**
     * Getter for the message timeout.
     * 
     * @return
     */
    public Integer getTimeout() {
        return mTimeout;
    }

    /**
     * Setter for the message timeout.
     * 
     * @param timeout
     */
    public void setTimeout(Integer timeout) {
        mTimeout = timeout;
    }

    /**
     * Return a readable representation of this object.
     * 
     * @return
     */
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("Message={");
        builder.append("type=").append(getType());
        builder.append(", ");
        builder.append("destination=").append(getDestination());
        builder.append(", ");
        builder.append("tag=").append(getTag());
        builder.append(", ");
        builder.append("data=").append(getData());
        builder.append(", ");
        builder.append("timeout=").append(getTimeout());
        builder.append("}");
        return builder.toString();
    }
}