package org.hurricane.driver.test;

import static junit.framework.Assert.assertEquals;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.hurricane.driver.Decoder;
import org.hurricane.driver.StreamEmulator;
import org.hurricane.driver.Utils;
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
import org.junit.Test;

/**
 * Runs all of the tests that make sure decoding of various data types works
 * properly.
 */
public class DecodeTest {
    /**
     * Test decoding an atom cache reference.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeAtomCacheRef() throws IOException {
        byte[] input = Utils.toBytes(131, 82, 1);
        AtomCacheRef expected = new AtomCacheRef((byte) 1);

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a byte.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeByte() throws IOException {
        byte[] input = Utils.toBytes(131, 97, 123);
        Integer expected = 123;

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding an integer.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeInteger() throws IOException {
        byte[] input = Utils.toBytes(131, 98, 0, 0, 4, 210);
        Integer expected = 1234;

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a negative integer.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeNegativeInteger() throws IOException {
        byte[] input = Utils.toBytes(131, 98, 255, 255, 255, 187);
        Integer expected = -69;

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a floating point integer.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeFloat() throws IOException {
        byte[] input = Utils.toBytes(131, 99, 49, 46, 49, 48, 48, 48, 48, 48,
                48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 56, 56, 56, 50, 101,
                43, 48, 48, 0, 0, 0, 0, 0);
        Double expected = new Double("1.1");

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding an atom.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeAtom() throws IOException {
        byte[] input = Utils.toBytes(131, 100, 0, 9, 105, 108, 105, 97, 95, 97,
                116, 111, 109);
        Atom expected = new Atom("ilia_atom");

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a reference.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeReference() throws IOException {
        byte[] input = Utils.toBytes(131, 101, 100, 0, 9, 105, 108, 105, 97,
                95, 97, 116, 111, 109, 1, 1, 1, 1, 42);
        Reference expected = new Reference(new Atom("ilia_atom"), 16843009,
                Byte.valueOf("42"));

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a port identifier.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodePort() throws IOException {
        byte[] input = Utils.toBytes(131, 102, 100, 0, 13, 110, 111, 110, 111,
                100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 1, 245, 0);
        Port expected = new Port(new Atom("nonode@nohost"), 501,
                Byte.valueOf("0"));

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a process identifier.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodePid() throws IOException {
        byte[] input = Utils.toBytes(131, 103, 100, 0, 13, 110, 111, 110, 111,
                100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0,
                0, 0, 0);
        Pid expected = new Pid(new Atom("nonode@nohost"), 31, 0,
                Byte.valueOf("0"));

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a small Tuple.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeSmallTuple() throws IOException {
        byte[] input = Utils.toBytes(131, 104, 2, 97, 42, 97, 69);
        Tuple expected = new Tuple(42, 69);

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a large Tuple.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeLargeTuple() throws IOException {
        byte[] input = Utils.toBytes(131, 105, 0, 0, 1, 26, 97, 42, 97, 69, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
                69);
        Tuple expected = new Tuple() {
            {
                elements().add(42);
                elements().add(69);
                for (Integer i = 0; i < 279; i++) {
                    elements().add(1);
                }
                elements().add(69);
            }
        };

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a nil/empty list.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeNil() throws IOException {
        byte[] input = Utils.toBytes(131, 106);
        Nil expected = new Nil();

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a string.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeString() throws IOException {
        byte[] input = Utils.toBytes(131, 107, 0, 24, 110, 111, 119, 32, 103,
                101, 116, 32, 121, 111, 117, 114, 32, 97, 115, 115, 32, 116,
                111, 32, 109, 97, 114, 115);
        String expected = "now get your ass to mars";

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a nil-terminated list.
     * 
     * @throws IOException
     */
    @Test
    @SuppressWarnings("serial")
    public void testDecodeListNil() throws IOException {
        byte[] input = Utils.toBytes(131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97,
                69, 97, 42, 106);
        List<Object> expected = new ArrayList<Object>() {
            {
                add(1024);
                add(69);
                add(42);
            }
        };

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a list with a trailer.
     * 
     * @throws IOException
     */
    @Test
    @SuppressWarnings("serial")
    public void testDecodeListTrailer() throws IOException {
        byte[] input = Utils.toBytes(131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97,
                69, 97, 42, 97, 1);
        List<Object> expected = new ArrayList<Object>() {
            {
                add(1024);
                add(69);
                add(42);
                add(1);
            }
        };

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding binary data.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeBinary() throws IOException {
        byte[] input = Utils.toBytes(131, 109, 0, 0, 0, 24, 110, 111, 119, 32,
                103, 101, 116, 32, 121, 111, 117, 114, 32, 97, 115, 115, 32,
                116, 111, 32, 109, 97, 114, 115);
        Binary expected = new Binary(
                new String("now get your ass to mars").getBytes());

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a large integer.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeSmallBigInteger() throws IOException {
        byte[] input = Utils.toBytes(131, 110, 5, 0, 5, 228, 183, 122, 4);
        BigInteger expected = new BigInteger("19238740997");

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a huge integer.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeLargeBigInteger() throws IOException {
        byte[] input = Utils.toBytes(131, 111, 0, 0, 1, 2, 0, 77, 166, 39, 216,
                70, 78, 172, 93, 52, 181, 124, 101, 170, 246, 159, 16, 153,
                154, 36, 214, 242, 85, 167, 60, 82, 247, 27, 245, 116, 125,
                142, 21, 240, 221, 108, 192, 132, 152, 51, 154, 161, 28, 103,
                117, 214, 153, 173, 30, 239, 99, 160, 203, 22, 156, 85, 168,
                87, 248, 230, 10, 115, 56, 202, 134, 245, 68, 118, 184, 171,
                238, 51, 180, 1, 81, 139, 164, 161, 186, 231, 229, 45, 235,
                244, 167, 92, 107, 159, 27, 27, 195, 22, 23, 236, 229, 112,
                138, 40, 150, 70, 107, 175, 46, 27, 67, 83, 124, 247, 18, 87,
                144, 167, 18, 212, 14, 129, 63, 190, 119, 149, 68, 165, 43,
                152, 211, 55, 3, 54, 145, 143, 121, 37, 88, 178, 78, 178, 210,
                187, 209, 95, 180, 82, 43, 15, 124, 42, 219, 102, 36, 195, 96,
                61, 131, 32, 224, 3, 92, 102, 31, 168, 163, 134, 238, 181, 13,
                36, 71, 28, 218, 181, 142, 191, 71, 241, 91, 166, 118, 178, 91,
                243, 2, 40, 76, 251, 97, 47, 34, 78, 233, 253, 137, 228, 231,
                176, 73, 165, 147, 17, 247, 147, 24, 66, 100, 211, 64, 134, 14,
                184, 130, 153, 249, 87, 2, 246, 140, 79, 220, 174, 26, 78, 214,
                64, 224, 88, 139, 150, 122, 184, 197, 160, 31, 203, 192, 9,
                112, 34, 66, 156, 248, 131, 70, 190, 215, 162, 230, 162, 74,
                181, 184, 36, 105, 229, 7, 168, 75, 47, 80, 83, 2);
        BigInteger expected = new BigInteger(
                "192387409991237409182730498172309487120398470129387401928374"
                        + "019283740918237409812730498560981723098461029384712093784098"
                        + "127364098712304987120394861023984701293561029387502983750921"
                        + "837409812734098123095620398571023948109274019238740912836750"
                        + "912837650981273054817230946120398571029386512983740591286350"
                        + "918276305918263059126305971260951623098712039857120936501293"
                        + "650912836750918263509281365091283650918263059812635091827365"
                        + "098172309012740129387409128374091283674091283764098127340981"
                        + "273049812734090238740918237409812730498172304987213094871230"
                        + "948712309487123098560129365095186320958612039586203956203985"
                        + "76019234870213977677");

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding an Erlang lambda created at run-time.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeNewFunction() throws IOException {
        byte[] input = Utils.toBytes(131, 112, 0, 0, 0, 71, 0, 184, 143, 129,
                148, 32, 145, 63, 179, 91, 201, 150, 94, 151, 97, 58, 227, 0,
                0, 0, 1, 0, 0, 0, 0, 115, 8, 101, 114, 108, 95, 101, 118, 97,
                108, 97, 20, 98, 4, 2, 194, 168, 103, 115, 13, 110, 111, 110,
                111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31,
                0, 0, 0, 0, 0);
        NewFunction expected = new NewFunction(Byte.valueOf("0"), new String(
                Utils.toBytes(184, 143, 129, 148, 32, 145, 63, 179, 91, 201,
                        150, 94, 151, 97, 58, 227)), 1, new Atom("erl_eval"),
                20, 67289768, new Pid(new Atom("nonode@nohost"), 31, 0,
                        Byte.valueOf("0")), new ArrayList<Object>());

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding an Erlang function.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeFunction() throws IOException {
        byte[] input = Utils.toBytes(131, 117, 0, 0, 0, 0, 103, 100, 0, 13,
                110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
                0, 0, 0, 31, 0, 0, 0, 0, 0, 100, 0, 9, 105, 108, 105, 97, 95,
                97, 116, 111, 109, 97, 1, 97, 1);
        ErlFunction expected = new ErlFunction(new Pid(
                new Atom("nonode@nohost"), 31, 0, Byte.valueOf("0")), new Atom(
                "ilia_atom"), 1, 1, new ArrayList<Object>());

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding an Erlang export specifier.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeExport() throws IOException {
        byte[] input = Utils.toBytes(131, 113, 100, 0, 6, 101, 114, 108, 97,
                110, 103, 100, 0, 4, 115, 101, 108, 102, 97, 0);
        Export expected = new Export(new Atom("erlang"), new Atom("self"),
                Byte.valueOf("0"));

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a new reference.
     * 
     * @throws IOException
     */
    @Test
    @SuppressWarnings("serial")
    public void testDecodeNewReference() throws IOException {
        byte[] input = Utils.toBytes(131, 114, 0, 3, 100, 0, 13, 110, 111, 110,
                111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0,
                94, 0, 0, 0, 0, 0, 0, 0, 0);
        NewReference expected = new NewReference(new Atom("nonode@nohost"),
                Byte.valueOf("0"), new ArrayList<Integer>() {
                    {
                        add(0);
                        add(0);
                        add(94);
                    }
                });

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a bit binary (space-compressed binary).
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeBitBinary() throws IOException {
        byte[] input = Utils.toBytes(131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64);
        BitBinary expected = new BitBinary(Byte.valueOf("4"), Utils.toBytes(
                195, 139, 30, 64));

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }

    /**
     * Test decoding a floating point number in the IEEE format.
     * 
     * @throws IOException
     */
    @Test
    public void testDecodeNewDouble() throws IOException {
        byte[] input = Utils.toBytes(131, 70, 1, 1, 1, 1, 1, 1, 1, 1);
        Double expected = new Double("8.321552E-317");

        StreamEmulator stream = new StreamEmulator(input);
        Object actual = Decoder.decode(stream);

        assertEquals(expected, actual);
    }
}