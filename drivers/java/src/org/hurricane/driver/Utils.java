package org.hurricane.driver;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hurricane.driver.datatypes.Tuple;

/**
 * Contains common utilities used throughout this adapter (mostly for working
 * with binary data).
 */
public class Utils {
    /**
     * Defines the value for a little endian architecture.
     */
    public static final Integer LITTLE_ENDIAN = 1;

    /**
     * Defines the value for a big endian architecture.
     */
    public static final Integer BIG_ENDIAN = 2;

    /**
     * Stores the endianness value detected for the current machine.
     */
    public static Integer ENDIANNESS;

    /**
     * Detect the machine endianness.
     */
    static {
        int t = 1 | 0 << 8 | 0 << 16 | 0 << 24;
        if (t == 1) {
            ENDIANNESS = LITTLE_ENDIAN;
        } else {
            ENDIANNESS = BIG_ENDIAN;
        }
    }

    /**
     * Compare two byte arrays for equality.
     * 
     * @param a
     * @param b
     * @return true if equal, false otherwise
     */
    public static Boolean compareBytes(byte[] a, byte[] b) {
        if (a.length != b.length) {
            return false;
        }

        for (Integer i = 0; i < a.length; i++) {
            if (a[i] != b[i]) {
                return false;
            }
        }

        return true;
    }

    /**
     * Make a string from the given bytes.
     * 
     * @param b
     * @return A string
     */
    public static String bytesToString(byte[] b) {
        StringBuilder builder = new StringBuilder();

        for (Integer i = 0; i < b.length; i++) {
            builder.append(b[i] & 0xff);
            builder.append(",");
        }

        return builder.toString();
    }

    /**
     * Reverse bytes in an array (used on little endian machines).
     * 
     * @param bytes
     * @return the reversed bytes
     */
    public static byte[] reverseBytes(byte[] bytes) {
        Byte swap;
        for (Integer i = 0; i < bytes.length / 2; i++) {
            swap = bytes[i];
            bytes[i] = bytes[bytes.length - i - 1];
            bytes[bytes.length - i - 1] = swap;
        }
        return bytes;
    }

    /**
     * Take a variadic number of bytes and return a single byte array of them.
     * 
     * @param is
     * @return
     */
    public static byte[] toBytes(int... is) {
        byte[] bs = new byte[is.length];
        for (Integer i = 0; i < is.length; i++) {
            bs[i] = (byte) is[i];
        }
        return bs;
    }

    /**
     * Converts bytes into an integer value.
     * 
     * @param bytes
     * @return the integer value
     */
    public static Long unpackNumber(byte[] bytes) {
        Long value = 0L;
        Integer shift = 0;
        if (ENDIANNESS == LITTLE_ENDIAN) {
            for (Integer i = bytes.length - 1; i >= 0; i--) {
                value |= (bytes[i] & 0xff) << (shift * 8);
                shift++;
            }
        } else {
            for (Integer i = 0; i < bytes.length; i++) {
                value |= (bytes[i] & 0xff) << (shift * 8);
                shift++;
            }
        }
        return value;
    }

    /**
     * Packs every type of primitive number into a big endian byte format.
     * 
     * @param number
     * @return the packed bytes
     * @throws UnsupportedOperationException
     */
    public static byte[] packNumber(Object number)
            throws UnsupportedOperationException {
        if (ENDIANNESS == LITTLE_ENDIAN) {
            if (number instanceof Byte) {
                byte full[] = new byte[1];
                full[0] = (byte) ((Byte) number);
                return full;
            } else if (number instanceof Short) {
                byte full[] = new byte[2];
                full[0] = (byte) ((Short) number >> 8);
                full[1] = (byte) ((Short) number >> 0);
                return full;
            } else if (number instanceof Integer) {
                byte full[] = new byte[4];
                full[0] = (byte) ((Integer) number >> 24);
                full[1] = (byte) ((Integer) number >> 16);
                full[2] = (byte) ((Integer) number >> 8);
                full[3] = (byte) ((Integer) number >> 0);
                return full;
            } else if (number instanceof Long) {
                byte full[] = new byte[8];
                full[0] = (byte) ((Long) number >> 56);
                full[1] = (byte) ((Long) number >> 48);
                full[2] = (byte) ((Long) number >> 40);
                full[3] = (byte) ((Long) number >> 32);
                full[4] = (byte) ((Long) number >> 24);
                full[5] = (byte) ((Long) number >> 16);
                full[6] = (byte) ((Long) number >> 8);
                full[7] = (byte) ((Long) number >> 0);
                return full;
            } else {
                throw new UnsupportedOperationException("Object " + number
                        + " cannot be packed!");
            }
        } else {
            if (number instanceof Byte) {
                byte full[] = new byte[1];
                full[0] = (byte) ((Byte) number);
                return full;
            } else if (number instanceof Short) {
                byte full[] = new byte[2];
                full[1] = (byte) ((Short) number >> 8);
                full[0] = (byte) ((Short) number >> 0);
                return full;
            } else if (number instanceof Integer) {
                byte full[] = new byte[4];
                full[3] = (byte) ((Integer) number >> 24);
                full[2] = (byte) ((Integer) number >> 16);
                full[1] = (byte) ((Integer) number >> 8);
                full[0] = (byte) ((Integer) number >> 0);
                return full;
            } else if (number instanceof Long) {
                byte full[] = new byte[8];
                full[7] = (byte) ((Long) number >> 56);
                full[6] = (byte) ((Long) number >> 48);
                full[5] = (byte) ((Long) number >> 40);
                full[4] = (byte) ((Long) number >> 32);
                full[3] = (byte) ((Long) number >> 24);
                full[2] = (byte) ((Long) number >> 16);
                full[1] = (byte) ((Long) number >> 8);
                full[0] = (byte) ((Long) number >> 0);
                return full;
            } else {
                throw new UnsupportedOperationException("Object " + number
                        + " cannot be packed!");
            }
        }
    }

    /**
     * Turn an Erlang-style property list into a map.
     * 
     * @param proplist
     *            The property list (usually a list of 2-tuples).
     * @return The generated map.
     */
    public static Map<Object, Object> proplistToMap(List<Tuple> proplist) {
        Map<Object, Object> result = new HashMap<Object, Object>();
        Iterator<Tuple> propIter = proplist.iterator();
        while (propIter.hasNext()) {
            Tuple element = propIter.next();
            if (element.elements().size() < 2) {
                throw new ArrayIndexOutOfBoundsException(
                        "Each Tuple in a proplist should have at least 2 elements");
            }
            result.put(element.elements().get(0), element.elements().get(1));
        }
        return result;
    }
}
