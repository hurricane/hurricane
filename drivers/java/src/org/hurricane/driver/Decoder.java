package org.hurricane.driver;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;

import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.AtomCacheRef;
import org.hurricane.driver.datatypes.Binary;
import org.hurricane.driver.datatypes.BitBinary;
import org.hurricane.driver.datatypes.ErlFunction;
import org.hurricane.driver.datatypes.Export;
import org.hurricane.driver.datatypes.NewFunction;
import org.hurricane.driver.datatypes.NewReference;
import org.hurricane.driver.datatypes.Nil;
import org.hurricane.driver.datatypes.Pid;
import org.hurricane.driver.datatypes.Port;
import org.hurricane.driver.datatypes.Reference;
import org.hurricane.driver.datatypes.Tuple;

/**
 * Implements all decoder functionality.
 */
public class Decoder {
    /**
     * Decode and return an Erlang atom cache ref.
     * 
     * @param stream
     * @return The atom cache ref
     * @throws IOException
     */
    public static AtomCacheRef decodeAtomCacheRef(StreamInterface stream)
            throws IOException {
        return new AtomCacheRef(stream.read(1)[0]);
    }

    /**
     * Decode and return a small integer.
     * 
     * @param stream
     * @return The small integer
     * @throws IOException
     */
    public static Integer decodeSmallIntegerExt(StreamInterface stream)
            throws IOException {
        Byte b = stream.read(1)[0];
        return b.intValue() & 0xff;
    }

    /**
     * Decode and return an integer.
     * 
     * @param stream
     * @return The integer
     * @throws IOException
     */
    public static Integer decodeIntegerExt(StreamInterface stream)
            throws IOException {
        return Utils.unpackNumber(stream.read(4)).intValue();
    }

    /**
     * Decode and return a float (represented by Erlang as a string).
     * 
     * @param stream
     * @return The double value
     * @throws IOException
     */
    public static Double decodeFloatExt(StreamInterface stream)
            throws IOException {
        return new Double(new String(stream.read(31)));
    }

    /**
     * Decode and return an Erlang atom.
     * 
     * @param stream
     * @return The atom
     * @throws IOException
     */
    public static Atom decodeAtomExt(StreamInterface stream) throws IOException {
        Integer atom_len = Utils.unpackNumber(stream.read(2)).intValue();
        return new Atom(new String(stream.read(atom_len)));
    }

    /**
     * Decode and return an Erlang reference.
     * 
     * @param stream
     * @return The reference
     * @throws IOException
     */
    public static Reference decodeReferenceExt(StreamInterface stream)
            throws IOException {
        Atom atom = (Atom) decode(stream, false);
        Integer identifier = Utils.unpackNumber(stream.read(4)).intValue();
        Byte creation = stream.read(1)[0];
        return new Reference(atom, identifier, creation);
    }

    /**
     * Decode and return an Erlang port.
     * 
     * @param stream
     * @return The port
     * @throws IOException
     */
    public static Port decodePortExt(StreamInterface stream) throws IOException {
        Atom atom = (Atom) decode(stream, false);
        Integer identifier = Utils.unpackNumber(stream.read(4)).intValue();
        Byte creation = stream.read(1)[0];
        return new Port(atom, identifier, creation);
    }

    /**
     * Decode and return an Erlang pid.
     * 
     * @param stream
     * @return The pid
     * @throws IOException
     */
    public static Pid decodePidExt(StreamInterface stream) throws IOException {
        Atom atom = (Atom) decode(stream, false);
        Integer identifier = Utils.unpackNumber(stream.read(4)).intValue();
        Integer serial = Utils.unpackNumber(stream.read(4)).intValue();
        Byte creation = stream.read(1)[0];
        return new Pid(atom, identifier, serial, creation);
    }

    /**
     * Decode and return a small Erlang tuple (fewer than 256 elements).
     * 
     * @param stream
     * @return The tuple
     * @throws IOException
     */
    public static Tuple decodeSmallTupleExt(StreamInterface stream)
            throws IOException {
        Short tupleLen = Utils.unpackNumber(stream.read(1)).shortValue();
        Tuple tuple = new Tuple(tupleLen.intValue());
        Object element;
        for (int i = 0; i < tupleLen; i++) {
            element = decode(stream, false);
            tuple.elements().add(element);
        }
        return tuple;
    }

    /**
     * Decode and return a large Erlang tuple (more than 256 elements).
     * 
     * @param stream
     * @return The tuple
     * @throws IOException
     */
    public static Tuple decodeLargeTupleExt(StreamInterface stream)
            throws IOException {
        Integer tupleLen = Utils.unpackNumber(stream.read(4)).intValue();
        Tuple tuple = new Tuple(tupleLen);
        Object element;
        for (int i = 0; i < tupleLen; i++) {
            element = decode(stream, false);
            tuple.elements().add(element);
        }
        return tuple;
    }

    /**
     * Decode an return a nil/null/None.
     * 
     * @param stream
     * @return An instance of Nil
     */
    public static Nil decodeNilExt(StreamInterface stream) {
        return new Nil();
    }

    /**
     * Decode and return a string.
     * 
     * @param stream
     * @return The string
     * @throws IOException
     */
    public static String decodeStringExt(StreamInterface stream)
            throws IOException {
        Integer strLen = Utils.unpackNumber(stream.read(2)).intValue();
        return new String(stream.read(strLen));
    }

    /**
     * Decode and return a list.
     * 
     * Depending on the list contents, a string may be returned. This will be
     * the case if the list contains only byte values, which means that the list
     * is actually intending to be a string, but being capped by Erlang's 65K
     * char limit for strings (before they overflow into a list).
     * 
     * @param stream
     * @return The list
     * @throws IOException
     */
    public static Object decodeListExt(StreamInterface stream)
            throws IOException {
        Integer listLen = Utils.unpackNumber(stream.read(4)).intValue();
        ArrayList<Object> list = new ArrayList<Object>(listLen);
        Object value;
        Boolean isStr = true;
        for (Integer i = 0; i < listLen; i++) {
            value = decode(stream, false);
            isStr = isStr && (value instanceof Byte);
            list.add(value);
        }
        Object tail = decode(stream, false);
        if (!(tail instanceof Nil)) {
            isStr = isStr && (tail instanceof Byte);
            list.add(tail);
        }
        if (isStr) {
            StringBuilder builder = new StringBuilder();
            for (Integer i = 0; i < list.size(); i++) {
                builder.append(list.get(i));
            }
            return builder.toString();
        } else {
            return list;
        }
    }

    /**
     * Decode and return an Erlang binary.
     * 
     * @param stream
     * @return The binary
     * @throws IOException
     */
    public static Binary decodeBinaryExt(StreamInterface stream)
            throws IOException {
        Integer binLen = Utils.unpackNumber(stream.read(4)).intValue();
        return new Binary(stream.read(binLen));
    }

    /**
     * Decode and return "small" big number.
     * 
     * @param stream
     * @return The big integer
     * @throws IOException
     */
    public static BigInteger decodeSmallBigExt(StreamInterface stream)
            throws IOException {
        Short numBytes = Utils.unpackNumber(stream.read(1)).shortValue();
        Byte sign = Utils.unpackNumber(stream.read(1)).byteValue();
        byte[] bytes = stream.read(numBytes);
        BigInteger value = new BigInteger(Utils.reverseBytes(bytes));
        if (sign == 1) {
            value = value.multiply(new BigInteger(new String("-1")));
        }
        return value;
    }

    /**
     * Decode and return "large" big number.
     * 
     * @param stream
     * @return The big integer
     * @throws IOException
     */
    public static BigInteger decodeLargeBigExt(StreamInterface stream)
            throws IOException {
        Long numBytes = Utils.unpackNumber(stream.read(4));
        Byte sign = Utils.unpackNumber(stream.read(1)).byteValue();
        byte[] bytes = stream.read(numBytes.intValue());
        BigInteger value = new BigInteger(Utils.reverseBytes(bytes));
        if (sign == 1) {
            value = value.multiply(new BigInteger(new String("-1")));
        }
        return value;
    }

    /**
     * Decode and return an Erlang "new function".
     * 
     * @param stream
     * @return The "new function"
     * @throws IOException
     */
    public static NewFunction decodeNewFunExt(StreamInterface stream)
            throws IOException {
        @SuppressWarnings("unused")
        Long size = Utils.unpackNumber(stream.read(4));

        Byte arity = Utils.unpackNumber(stream.read(1)).byteValue();
        String uniq = new String(stream.read(16));
        Integer index = Utils.unpackNumber(stream.read(4)).intValue();
        Long numFree = Utils.unpackNumber(stream.read(4));
        Object module = decode(stream, false);
        Object oldIndex = decode(stream, false);
        Object oldUniq = decode(stream, false);
        Pid pid = (Pid) decode(stream, false);
        ArrayList<Object> freeVars = new ArrayList<Object>();

        Object freeVar;
        for (Integer i = 0; i < numFree; i++) {
            freeVar = decode(stream, false);
            freeVars.add(freeVar);
        }

        return new NewFunction(arity, uniq, index, module, oldIndex, oldUniq,
                pid, freeVars);
    }

    /**
     * Decode and return a small Erlang atom.
     * 
     * @param stream
     * @return The atom
     * @throws IOException
     */
    public static Atom decodeSmallAtomExt(StreamInterface stream)
            throws IOException {
        Integer atomLen = Utils.unpackNumber(stream.read(1)).intValue();
        String atomName = new String(stream.read(atomLen));
        return new Atom(atomName);
    }

    /**
     * Decode and return an Erlang function.
     * 
     * @param stream
     * @return The function
     * @throws IOException
     */
    public static ErlFunction decodeFunExt(StreamInterface stream)
            throws IOException {
        Long numFree = Utils.unpackNumber(stream.read(4));
        Pid pid = (Pid) decode(stream, false);
        Object module = decode(stream, false);
        Object index = decode(stream, false);
        Object uniq = decode(stream, false);
        ArrayList<Object> freeVars = new ArrayList<Object>();

        Object freeVar;
        for (Integer i = 0; i < numFree; i++) {
            freeVar = decode(stream, false);
            freeVars.add(freeVar);
        }

        return new ErlFunction(pid, module, index, uniq, freeVars);
    }

    /**
     * Decode and return an Erlang export.
     * 
     * @param stream
     * @return The export
     * @throws IOException
     */
    public static Export decodeExportExt(StreamInterface stream)
            throws IOException {
        Object module = decode(stream, false);
        Object function = decode(stream, false);
        Byte arity = ((Integer) decode(stream, false)).byteValue();

        return new Export(module, function, arity);
    }

    /**
     * Decode and return an Erlang "new reference".
     * 
     * @param stream
     * @return The "new reference"
     * @throws IOException
     */
    public static NewReference decodeNewReferenceExt(StreamInterface stream)
            throws IOException {
        Integer length = Utils.unpackNumber(stream.read(2)).intValue();
        Object atom = decode(stream, false);
        Byte creation = Utils.unpackNumber(stream.read(1)).byteValue();
        Integer[] revIds = new Integer[length];
        for (Integer i = 0; i < length; i++) {
            revIds[i] = Utils.unpackNumber(stream.read(4)).intValue();
        }
        ArrayList<Integer> identifiers = new ArrayList<Integer>(length);
        for (Integer i = length - 1; i >= 0; i--) {
            identifiers.add(revIds[i]);
        }
        return new NewReference(atom, creation, identifiers);
    }

    /**
     * Decode and return an Erlang bit binary.
     * 
     * @param stream
     * @return The bit binary
     * @throws IOException
     */
    public static BitBinary decodeBitBinaryExt(StreamInterface stream)
            throws IOException {
        Integer length = Utils.unpackNumber(stream.read(4)).intValue();
        return new BitBinary(Utils.unpackNumber(stream.read(1)).byteValue(),
                stream.read(length));
    }

    /**
     * Decode and return an IEEE 8-byte floating-point number.
     * 
     * @param stream
     * @return The double value
     * @throws IOException
     */
    public static Double decodeNewFloatExt(StreamInterface stream)
            throws IOException {
        Long value = Utils.unpackNumber(stream.read(8));
        return Double.longBitsToDouble(value);
    }

    /**
     * Decode and return an IEEE 8-byte floating-point number.
     * 
     * @param stream
     * @return The decoded value
     * @throws UnsupportedOperationException
     * @throws IOException
     */
    public static Object decode(StreamInterface stream)
            throws UnsupportedOperationException, IOException {
        return decode(stream, true);
    }

    /**
     * Decode and return an IEEE 8-byte floating-point number.
     * 
     * If checkDistTag, check to see that the first byte is 131 (this is how
     * Erlang flags the beginning of every data type). This check does not need
     * to be performed when recursively decoding nested data types.
     * 
     * @param stream
     * @param checkDistTag
     * @return The decoded value
     * @throws UnsupportedOperationException
     * @throws IOException
     */
    public static Object decode(StreamInterface stream, Boolean checkDistTag)
            throws UnsupportedOperationException, IOException {
        byte firstByte = stream.read(1)[0];
        byte extCode;
        if (checkDistTag) {
            if (firstByte != (byte) 131) {
                throw new UnsupportedOperationException(
                        "this is not an Erlang EXT datatype");
            } else {
                extCode = stream.read(1)[0];
            }
        } else {
            extCode = firstByte;
        }

        switch (extCode) {
        case 70:
            return decodeNewFloatExt(stream);
        case 77:
            return decodeBitBinaryExt(stream);
        case 82:
            return decodeAtomCacheRef(stream);
        case 97:
            return decodeSmallIntegerExt(stream);
        case 98:
            return decodeIntegerExt(stream);
        case 99:
            return decodeFloatExt(stream);
        case 100:
            return decodeAtomExt(stream);
        case 101:
            return decodeReferenceExt(stream);
        case 102:
            return decodePortExt(stream);
        case 103:
            return decodePidExt(stream);
        case 104:
            return decodeSmallTupleExt(stream);
        case 105:
            return decodeLargeTupleExt(stream);
        case 106:
            return decodeNilExt(stream);
        case 107:
            return decodeStringExt(stream);
        case 108:
            return decodeListExt(stream);
        case 109:
            return decodeBinaryExt(stream);
        case 110:
            return decodeSmallBigExt(stream);
        case 111:
            return decodeLargeBigExt(stream);
        case 112:
            return decodeNewFunExt(stream);
        case 113:
            return decodeExportExt(stream);
        case 114:
            return decodeNewReferenceExt(stream);
        case 115:
            return decodeSmallAtomExt(stream);
        case 117:
            return decodeFunExt(stream);
        default:
            throw new UnsupportedOperationException(
                    "Unable to decode Erlang EXT data type: " + extCode);
        }
    }
}
