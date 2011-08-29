package org.hurricane.driver;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
 * Implements all encoder functionality.
 */
public class Encoder {
    /**
     * Encode a floating-point number into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeDouble(Double data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(70));
        Long value = Double.doubleToLongBits(data);
        stream.write(Utils.packNumber(value));
    }

    /**
     * Encode an Erlang bit binary into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeBitBinary(BitBinary data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(77));
        byte[] binary = data.mData.getBytes();
        stream.write(Utils.packNumber(binary.length));
        stream.write(Utils.toBytes(data.mBits));
        stream.write(binary);
    }

    /**
     * Encode an Erlang atom cache ref into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeAtomCacheRef(AtomCacheRef data,
            StreamInterface stream) throws IOException {
        stream.write(Utils.toBytes(82));
        stream.write(Utils.toBytes((int) data.mValue));
    }

    /**
     * Encode a small integer (byte) into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeByte(Byte data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(97));
        stream.write(Utils.toBytes((int) data));
    }

    /**
     * Encode a small integer (short) into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeShort(Short data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(98));
        stream.write(Utils.packNumber(data.intValue()));
    }

    /**
     * Encode an integer into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeInteger(Integer data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(98));
        stream.write(Utils.packNumber(data));
    }

    /**
     * Encode a large integer (long) into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeLong(Long data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(110));
        BigInteger value = BigInteger.valueOf(data);
        byte[] bytes = value.toByteArray();

        stream.write(Utils.toBytes(bytes.length));
        if (value.compareTo(new BigInteger("0")) == -1) {
            stream.write(Utils.toBytes(1));
        } else {
            stream.write(Utils.toBytes(0));
        }
        stream.write(Utils.reverseBytes(bytes));
    }

    /**
     * Encode a BigInteger into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeBigInteger(BigInteger data, StreamInterface stream)
            throws IOException {
        byte[] bytes = data.toByteArray();

        if (bytes.length < 256) {
            stream.write(Utils.toBytes(110));
            stream.write(Utils.toBytes(bytes.length));
        } else {
            stream.write(Utils.toBytes(111));
            stream.write(Utils.packNumber(bytes.length));
        }

        if (data.compareTo(new BigInteger("0")) == -1) {
            stream.write(Utils.toBytes(1));
        } else {
            stream.write(Utils.toBytes(0));
        }
        stream.write(Utils.reverseBytes(bytes));
    }

    /**
     * Encode an atom into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeAtom(Atom data, StreamInterface stream)
            throws IOException {
        byte[] bytes = data.mName.getBytes();
        if (bytes.length < 0xf) {
            stream.write(Utils.toBytes(115));
            stream.write(Utils.toBytes(bytes.length));
        } else {
            stream.write(Utils.toBytes(100));
            stream.write(Utils.packNumber(new Integer(bytes.length)
                    .shortValue()));
        }
        stream.write(bytes);
    }

    /**
     * Encode an Erlang reference into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeReference(Reference data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(101));
        encode(data.mAtom, stream, false);
        stream.write(Utils.packNumber(data.mIdentifier));
        stream.write(Utils.toBytes((int) data.mCreation));
    }

    /**
     * Encode an Erlang port into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodePort(Port data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(102));
        encode(data.mAtom, stream, false);
        stream.write(Utils.packNumber(data.mIdentifier));
        stream.write(Utils.toBytes((int) data.mCreation));
    }

    /**
     * Encode an Erlang pid into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodePid(Pid data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(103));
        encode(data.mAtom, stream, false);
        stream.write(Utils.packNumber(data.mIdentifier));
        stream.write(Utils.packNumber(data.mSerial));
        stream.write(Utils.toBytes((int) data.mCreation));
    }

    /**
     * Encode a Tuple into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeTuple(Tuple data, StreamInterface stream)
            throws IOException {
        int size = data.mElements.size();
        if (size < 256) {
            stream.write(Utils.toBytes(104));
            stream.write(Utils.toBytes(size));
        } else {
            stream.write(Utils.toBytes(105));
            stream.write(Utils.packNumber(size));
        }

        for (Integer i = 0; i < size; i++) {
            encode(data.mElements.get(i), stream, false);
        }
    }

    /**
     * Encode a nil/null into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeNil(Nil data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(106));
    }

    /**
     * Encode an Erlang binary into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeBinary(Binary data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(109));
        stream.write(Utils.packNumber(data.mData.length));
        stream.write(data.mData);
    }

    /**
     * Encode a string into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeString(String data, StreamInterface stream)
            throws IOException {
        Integer strLen = data.length();
        if (strLen > 0xffff) {
            stream.write(Utils.toBytes(108));
            stream.write(Utils.packNumber(strLen));
            stream.write(data.getBytes());
            stream.write(Utils.toBytes(106));
        } else {
            stream.write(Utils.toBytes(107));
            stream.write(Utils.packNumber(strLen.shortValue()));
            stream.write(data.getBytes());
        }
    }

    /**
     * Encode a "new reference" into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeNewReference(NewReference data,
            StreamInterface stream) throws IOException {
        stream.write(Utils.toBytes(114));
        Short idsLen = ((Integer) data.mIds.size()).shortValue();
        stream.write(Utils.packNumber(idsLen));
        encode(data.mAtom, stream, false);
        stream.write(Utils.toBytes((int) data.mCreation));
        for (Short i = 0; i < idsLen; i++) {
            stream.write(Utils.packNumber(data.mIds.get(i)));
        }
    }

    /**
     * Encode an Erlang function into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeErlFunction(ErlFunction data,
            StreamInterface stream) throws IOException {
        stream.write(Utils.toBytes(117));
        Integer fvLen = data.mFreeVars.size();
        stream.write(Utils.packNumber(fvLen));
        encode(data.mPid, stream, false);
        encode(data.mModule, stream, false);
        encode(data.mIndex, stream, false);
        encode(data.mUniq, stream, false);

        for (Integer i = 0; i < fvLen; i++) {
            stream.write(Utils.packNumber(data.mFreeVars.get(i)));
        }
    }

    /**
     * Encode an Erlang "new function" into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeNewFunction(NewFunction data,
            StreamInterface stream) throws IOException {
        stream.write(Utils.toBytes(112));
        Integer fvLen = data.mFreeVars.size();

        StreamEmulator buffer = new StreamEmulator();
        buffer.write(Utils.toBytes((int) data.mArity));
        buffer.write(data.mUniq.getBytes());
        buffer.write(Utils.packNumber(data.mIndex));
        buffer.write(Utils.packNumber(fvLen));
        encode(data.mModule, buffer, false);
        encode(data.mOldIndex, buffer, false);
        encode(data.mOldUniq, buffer, false);
        encode(data.mPid, buffer, false);

        for (Integer i = 0; i < fvLen; i++) {
            buffer.write(Utils.packNumber(data.mFreeVars.get(i)));
        }

        stream.write(Utils.packNumber(buffer.size() + 4));
        stream.write(buffer.getBytes());
    }

    /**
     * Encode an Erlang export into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeExport(Export data, StreamInterface stream)
            throws IOException {
        stream.write(Utils.toBytes(113));
        encode(data.mModule, stream, false);
        encode(data.mFunction, stream, false);
        encode(data.mArity, stream, false);
    }

    /**
     * Encode a list into the stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeList(List<Object> data, StreamInterface stream)
            throws IOException {
        Integer listLen = data.size();
        stream.write(Utils.toBytes(108));
        stream.write(Utils.packNumber(listLen));
        Iterator<Object> it = data.iterator();
        while (it.hasNext()) {
            encode(it.next(), stream, false);
        }
        stream.write(Utils.toBytes(106));
    }

    /**
     * Encode a map into the stream (as a property list).
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encodeMap(Map<Object, Object> data,
            StreamInterface stream) throws IOException {
        ArrayList<Tuple> propList = new ArrayList<Tuple>();
        Set<Object> keys = data.keySet();
        Iterator<Object> it = keys.iterator();

        Object key;
        Object value;
        Tuple entry;
        while (it.hasNext()) {
            key = it.next();
            value = data.get(key);
            entry = new Tuple();
            entry.elements().add(key);
            entry.elements().add(value);
            propList.add(entry);
        }

        encode(propList, stream, false);
    }

    /**
     * Encode the given data into the given stream.
     * 
     * @param data
     * @param stream
     * @throws IOException
     */
    public static void encode(Object data, StreamInterface stream)
            throws IOException {
        encode(data, stream, true);
    }

    /**
     * Encode the given data into the given stream.
     * 
     * If sendMagicByte, the value 131 is sent before anything (this is how
     * Erlang denotes that there is a new piece of data coming across). However,
     * for nested data, this only needs to be sent once.
     * 
     * @param data
     * @param stream
     * @param sendMagicByte
     * @throws UnsupportedOperationException
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    public static void encode(Object data, StreamInterface stream,
            Boolean sendMagicByte) throws UnsupportedOperationException,
            IOException {
        if (sendMagicByte) {
            stream.write(Utils.toBytes(131));
        }

        Class<? extends Object> dc = data.getClass();
        if (dc == Float.class) {
            encodeDouble(((Float) data).doubleValue(), stream);
        } else if (dc == Double.class) {
            encodeDouble((Double) data, stream);
        } else if (dc == BitBinary.class) {
            encodeBitBinary((BitBinary) data, stream);
        } else if (dc == AtomCacheRef.class) {
            encodeAtomCacheRef((AtomCacheRef) data, stream);
        } else if (dc == Byte.class) {
            encodeByte((Byte) data, stream);
        } else if (dc == Short.class) {
            encodeShort((Short) data, stream);
        } else if (dc == Integer.class) {
            encodeInteger((Integer) data, stream);
        } else if (dc == Long.class) {
            encodeLong((Long) data, stream);
        } else if (dc == BigInteger.class) {
            encodeBigInteger((BigInteger) data, stream);
        } else if (dc == Atom.class) {
            encodeAtom((Atom) data, stream);
        } else if (dc == Reference.class) {
            encodeReference((Reference) data, stream);
        } else if (dc == Port.class) {
            encodePort((Port) data, stream);
        } else if (dc == Pid.class) {
            encodePid((Pid) data, stream);
        } else if (dc == Tuple.class) {
            encodeTuple((Tuple) data, stream);
        } else if (dc == Nil.class) {
            encodeNil((Nil) data, stream);
        } else if (dc == String.class) {
            encodeString((String) data, stream);
        } else if (dc == Binary.class) {
            encodeBinary((Binary) data, stream);
        } else if (dc == NewReference.class) {
            encodeNewReference((NewReference) data, stream);
        } else if (dc == ErlFunction.class) {
            encodeErlFunction((ErlFunction) data, stream);
        } else if (dc == NewFunction.class) {
            encodeNewFunction((NewFunction) data, stream);
        } else if (dc == Export.class) {
            encodeExport((Export) data, stream);
        } else if (data instanceof List) {
            encodeList((List<Object>) data, stream);
        } else if (data instanceof Map) {
            encodeMap((Map<Object, Object>) data, stream);
        } else {
            throw new UnsupportedOperationException(dc.getName()
                    + " is not Erlang serializable!");
        }
    }
}
