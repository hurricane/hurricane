package org.hurricane.driver.test;

import static org.junit.Assert.assertArrayEquals;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hurricane.driver.Encoder;
import org.hurricane.driver.StreamEmulator;
import org.hurricane.driver.Utils;
import org.hurricane.driver.datatypes.Atom;
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
 * Runs all of the tests that make sure encoding of various data types works
 * properly.
 */
public class EncodeTest {
    /**
     * Test encoding a Double.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeDouble() throws IOException {
        Double input = Double.valueOf("1.1");
        byte[] expected = Utils.toBytes(131, 70, 63, 241, 153, 153, 153, 153,
                153, 154);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a bit binary (space-compressed binary).
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeBitBinary() throws IOException {
        BitBinary input = new BitBinary(Byte.valueOf("4"), Utils.toBytes(195,
                139, 30, 64));
        byte[] expected = Utils.toBytes(131, 77, 0, 0, 0, 4, 4, 195, 139, 30,
                64);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a byte.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeByte() throws IOException {
        Byte input = Byte.valueOf("1");
        byte[] expected = Utils.toBytes(131, 97, 1);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a short integer.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeShort() throws IOException {
        Short input = Short.valueOf("1");
        byte[] expected = Utils.toBytes(131, 98, 0, 0, 0, 1);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a regular integer.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeInteger() throws IOException {
        Integer input = Integer.valueOf("1");
        byte[] expected = Utils.toBytes(131, 98, 0, 0, 0, 1);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a long integer.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeLong() throws IOException {
        Long input = 7446744073709551614L;
        byte[] expected = Utils.toBytes(131, 110, 8, 0, 254, 255, 179, 206, 71,
                38, 88, 103);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a big integer.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeSmallBigInteger() throws IOException {
        BigInteger input = new BigInteger("69").pow(69);
        byte[] expected = Utils.toBytes(131, 110, 53, 0, 117, 116, 129, 23,
                136, 197, 28, 142, 23, 72, 229, 165, 191, 192, 22, 188, 155,
                206, 43, 113, 63, 242, 3, 25, 228, 209, 17, 66, 83, 52, 125,
                56, 245, 58, 162, 52, 201, 215, 74, 188, 3, 203, 142, 87, 67,
                19, 43, 193, 5, 110, 196, 226, 44);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a huge integer.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeLargeBigInteger() throws IOException {
        BigInteger input = new BigInteger("-69").pow(696);
        byte[] expected = Utils.toBytes(131, 111, 0, 0, 2, 20, 0, 33, 89, 30,
                229, 196, 107, 139, 7, 211, 176, 94, 210, 240, 73, 193, 252,
                228, 16, 46, 39, 41, 172, 134, 15, 148, 175, 202, 174, 39, 93,
                95, 64, 112, 61, 184, 30, 213, 51, 0, 148, 67, 252, 76, 57, 30,
                101, 112, 72, 159, 222, 69, 12, 72, 158, 21, 174, 51, 215, 253,
                188, 108, 235, 36, 137, 63, 135, 237, 38, 12, 135, 171, 231,
                225, 245, 246, 107, 70, 145, 37, 35, 181, 217, 230, 237, 151,
                244, 238, 71, 140, 158, 2, 236, 2, 93, 139, 192, 182, 2, 50,
                113, 43, 5, 236, 64, 243, 200, 20, 89, 177, 118, 93, 105, 30,
                252, 155, 221, 247, 170, 178, 142, 230, 55, 167, 20, 34, 188,
                215, 141, 245, 9, 222, 240, 100, 209, 43, 38, 114, 13, 194,
                226, 173, 132, 73, 15, 237, 138, 30, 190, 247, 101, 97, 157,
                20, 252, 135, 90, 24, 201, 203, 85, 23, 246, 227, 235, 87, 138,
                230, 66, 48, 114, 50, 107, 197, 216, 154, 225, 22, 25, 242,
                197, 212, 131, 185, 132, 162, 210, 65, 232, 14, 213, 216, 36,
                126, 216, 164, 16, 1, 74, 252, 199, 36, 155, 160, 252, 165,
                218, 85, 222, 27, 180, 119, 88, 156, 92, 20, 184, 163, 209,
                123, 104, 43, 183, 108, 101, 228, 132, 16, 35, 210, 22, 245,
                68, 110, 45, 82, 22, 142, 52, 122, 7, 105, 14, 206, 113, 124,
                70, 167, 77, 225, 59, 253, 180, 181, 216, 137, 245, 9, 5, 80,
                206, 16, 12, 185, 153, 155, 248, 201, 114, 96, 179, 184, 11,
                186, 233, 212, 121, 204, 235, 191, 214, 163, 41, 83, 252, 84,
                84, 56, 55, 60, 119, 101, 99, 167, 129, 41, 23, 111, 107, 225,
                103, 79, 81, 142, 19, 236, 207, 115, 250, 103, 206, 126, 5, 43,
                201, 60, 114, 237, 13, 111, 210, 211, 204, 247, 189, 195, 138,
                2, 41, 139, 167, 180, 56, 59, 180, 225, 93, 12, 69, 20, 144,
                251, 15, 59, 155, 248, 59, 191, 181, 1, 221, 181, 69, 62, 218,
                157, 173, 43, 160, 213, 22, 155, 15, 1, 173, 97, 115, 172, 243,
                209, 234, 152, 42, 29, 161, 150, 134, 41, 197, 232, 51, 208,
                97, 230, 236, 83, 240, 74, 171, 221, 6, 125, 70, 51, 42, 185,
                243, 237, 232, 105, 145, 114, 181, 138, 130, 0, 139, 216, 10,
                137, 96, 156, 133, 150, 212, 79, 213, 5, 170, 161, 40, 80, 163,
                135, 65, 51, 2, 176, 182, 10, 25, 239, 40, 22, 192, 166, 213,
                250, 238, 175, 244, 201, 53, 0, 63, 196, 29, 36, 59, 159, 202,
                175, 18, 85, 170, 114, 224, 209, 94, 172, 65, 182, 244, 181,
                166, 29, 177, 235, 21, 228, 10, 247, 170, 185, 42, 151, 195,
                33, 255, 77, 5, 166, 98, 50, 4, 20, 44, 227, 203, 198, 212, 3,
                112, 48, 102, 20, 142, 238, 92, 183, 111, 39, 7, 81, 72, 166,
                102, 182, 15, 46, 10, 74, 211, 137, 20, 168, 211, 1, 105, 1,
                180, 151, 185, 129, 59, 77, 71, 42, 88, 31, 94, 147, 11);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding an atom.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeAtom() throws IOException {
        Atom input = new Atom("ilia_atom");
        byte[] expected = Utils.toBytes(131, 115, 9, 105, 108, 105, 97, 95, 97,
                116, 111, 109);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding an atom with a long identifier.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeLongAtom() throws IOException {
        Atom input = new Atom("ilia_atom_is_quite_a_long_atom");
        byte[] expected = Utils.toBytes(131, 100, 0, 30, 105, 108, 105, 97, 95,
                97, 116, 111, 109, 95, 105, 115, 95, 113, 117, 105, 116, 101,
                95, 97, 95, 108, 111, 110, 103, 95, 97, 116, 111, 109);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a reference.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeReference() throws IOException {
        Reference input = new Reference(new Atom("nonode@nohost"), 69,
                Byte.valueOf("0"));
        byte[] expected = Utils.toBytes(131, 101, 115, 13, 110, 111, 110, 111,
                100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 69, 0);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a port identifier.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodePort() throws IOException {
        Port input = new Port(new Atom("nonode@nohost"), 69, Byte.valueOf("0"));
        byte[] expected = Utils.toBytes(131, 102, 115, 13, 110, 111, 110, 111,
                100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 69, 0);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a process identifier.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodePid() throws IOException {
        Pid input = new Pid(new Atom("nonode@nohost"), 69, 0, Byte.valueOf("0"));
        byte[] expected = Utils.toBytes(131, 103, 115, 13, 110, 111, 110, 111,
                100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 69, 0, 0,
                0, 0, 0);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a small tuple.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeSmallTuple() throws IOException {
        Tuple input = new Tuple();
        for (Integer i = 0; i < 10; i++) {
            input.elements().add(i.byteValue());
        }

        byte[] expected = Utils.toBytes(131, 104, 10, 97, 0, 97, 1, 97, 2, 97,
                3, 97, 4, 97, 5, 97, 6, 97, 7, 97, 8, 97, 9);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a large tuple.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeLargeTuple() throws IOException {
        Tuple input = new Tuple();
        for (Integer i = 0; i < 400; i++) {
            if (i < 256) {
                input.elements().add(i.byteValue());
            } else {
                input.elements().add(i);
            }
        }

        byte[] expected = Utils.toBytes(131, 105, 0, 0, 1, 144, 97, 0, 97, 1,
                97, 2, 97, 3, 97, 4, 97, 5, 97, 6, 97, 7, 97, 8, 97, 9, 97, 10,
                97, 11, 97, 12, 97, 13, 97, 14, 97, 15, 97, 16, 97, 17, 97, 18,
                97, 19, 97, 20, 97, 21, 97, 22, 97, 23, 97, 24, 97, 25, 97, 26,
                97, 27, 97, 28, 97, 29, 97, 30, 97, 31, 97, 32, 97, 33, 97, 34,
                97, 35, 97, 36, 97, 37, 97, 38, 97, 39, 97, 40, 97, 41, 97, 42,
                97, 43, 97, 44, 97, 45, 97, 46, 97, 47, 97, 48, 97, 49, 97, 50,
                97, 51, 97, 52, 97, 53, 97, 54, 97, 55, 97, 56, 97, 57, 97, 58,
                97, 59, 97, 60, 97, 61, 97, 62, 97, 63, 97, 64, 97, 65, 97, 66,
                97, 67, 97, 68, 97, 69, 97, 70, 97, 71, 97, 72, 97, 73, 97, 74,
                97, 75, 97, 76, 97, 77, 97, 78, 97, 79, 97, 80, 97, 81, 97, 82,
                97, 83, 97, 84, 97, 85, 97, 86, 97, 87, 97, 88, 97, 89, 97, 90,
                97, 91, 97, 92, 97, 93, 97, 94, 97, 95, 97, 96, 97, 97, 97, 98,
                97, 99, 97, 100, 97, 101, 97, 102, 97, 103, 97, 104, 97, 105,
                97, 106, 97, 107, 97, 108, 97, 109, 97, 110, 97, 111, 97, 112,
                97, 113, 97, 114, 97, 115, 97, 116, 97, 117, 97, 118, 97, 119,
                97, 120, 97, 121, 97, 122, 97, 123, 97, 124, 97, 125, 97, 126,
                97, 127, 97, 128, 97, 129, 97, 130, 97, 131, 97, 132, 97, 133,
                97, 134, 97, 135, 97, 136, 97, 137, 97, 138, 97, 139, 97, 140,
                97, 141, 97, 142, 97, 143, 97, 144, 97, 145, 97, 146, 97, 147,
                97, 148, 97, 149, 97, 150, 97, 151, 97, 152, 97, 153, 97, 154,
                97, 155, 97, 156, 97, 157, 97, 158, 97, 159, 97, 160, 97, 161,
                97, 162, 97, 163, 97, 164, 97, 165, 97, 166, 97, 167, 97, 168,
                97, 169, 97, 170, 97, 171, 97, 172, 97, 173, 97, 174, 97, 175,
                97, 176, 97, 177, 97, 178, 97, 179, 97, 180, 97, 181, 97, 182,
                97, 183, 97, 184, 97, 185, 97, 186, 97, 187, 97, 188, 97, 189,
                97, 190, 97, 191, 97, 192, 97, 193, 97, 194, 97, 195, 97, 196,
                97, 197, 97, 198, 97, 199, 97, 200, 97, 201, 97, 202, 97, 203,
                97, 204, 97, 205, 97, 206, 97, 207, 97, 208, 97, 209, 97, 210,
                97, 211, 97, 212, 97, 213, 97, 214, 97, 215, 97, 216, 97, 217,
                97, 218, 97, 219, 97, 220, 97, 221, 97, 222, 97, 223, 97, 224,
                97, 225, 97, 226, 97, 227, 97, 228, 97, 229, 97, 230, 97, 231,
                97, 232, 97, 233, 97, 234, 97, 235, 97, 236, 97, 237, 97, 238,
                97, 239, 97, 240, 97, 241, 97, 242, 97, 243, 97, 244, 97, 245,
                97, 246, 97, 247, 97, 248, 97, 249, 97, 250, 97, 251, 97, 252,
                97, 253, 97, 254, 97, 255, 98, 0, 0, 1, 0, 98, 0, 0, 1, 1, 98,
                0, 0, 1, 2, 98, 0, 0, 1, 3, 98, 0, 0, 1, 4, 98, 0, 0, 1, 5, 98,
                0, 0, 1, 6, 98, 0, 0, 1, 7, 98, 0, 0, 1, 8, 98, 0, 0, 1, 9, 98,
                0, 0, 1, 10, 98, 0, 0, 1, 11, 98, 0, 0, 1, 12, 98, 0, 0, 1, 13,
                98, 0, 0, 1, 14, 98, 0, 0, 1, 15, 98, 0, 0, 1, 16, 98, 0, 0, 1,
                17, 98, 0, 0, 1, 18, 98, 0, 0, 1, 19, 98, 0, 0, 1, 20, 98, 0,
                0, 1, 21, 98, 0, 0, 1, 22, 98, 0, 0, 1, 23, 98, 0, 0, 1, 24,
                98, 0, 0, 1, 25, 98, 0, 0, 1, 26, 98, 0, 0, 1, 27, 98, 0, 0, 1,
                28, 98, 0, 0, 1, 29, 98, 0, 0, 1, 30, 98, 0, 0, 1, 31, 98, 0,
                0, 1, 32, 98, 0, 0, 1, 33, 98, 0, 0, 1, 34, 98, 0, 0, 1, 35,
                98, 0, 0, 1, 36, 98, 0, 0, 1, 37, 98, 0, 0, 1, 38, 98, 0, 0, 1,
                39, 98, 0, 0, 1, 40, 98, 0, 0, 1, 41, 98, 0, 0, 1, 42, 98, 0,
                0, 1, 43, 98, 0, 0, 1, 44, 98, 0, 0, 1, 45, 98, 0, 0, 1, 46,
                98, 0, 0, 1, 47, 98, 0, 0, 1, 48, 98, 0, 0, 1, 49, 98, 0, 0, 1,
                50, 98, 0, 0, 1, 51, 98, 0, 0, 1, 52, 98, 0, 0, 1, 53, 98, 0,
                0, 1, 54, 98, 0, 0, 1, 55, 98, 0, 0, 1, 56, 98, 0, 0, 1, 57,
                98, 0, 0, 1, 58, 98, 0, 0, 1, 59, 98, 0, 0, 1, 60, 98, 0, 0, 1,
                61, 98, 0, 0, 1, 62, 98, 0, 0, 1, 63, 98, 0, 0, 1, 64, 98, 0,
                0, 1, 65, 98, 0, 0, 1, 66, 98, 0, 0, 1, 67, 98, 0, 0, 1, 68,
                98, 0, 0, 1, 69, 98, 0, 0, 1, 70, 98, 0, 0, 1, 71, 98, 0, 0, 1,
                72, 98, 0, 0, 1, 73, 98, 0, 0, 1, 74, 98, 0, 0, 1, 75, 98, 0,
                0, 1, 76, 98, 0, 0, 1, 77, 98, 0, 0, 1, 78, 98, 0, 0, 1, 79,
                98, 0, 0, 1, 80, 98, 0, 0, 1, 81, 98, 0, 0, 1, 82, 98, 0, 0, 1,
                83, 98, 0, 0, 1, 84, 98, 0, 0, 1, 85, 98, 0, 0, 1, 86, 98, 0,
                0, 1, 87, 98, 0, 0, 1, 88, 98, 0, 0, 1, 89, 98, 0, 0, 1, 90,
                98, 0, 0, 1, 91, 98, 0, 0, 1, 92, 98, 0, 0, 1, 93, 98, 0, 0, 1,
                94, 98, 0, 0, 1, 95, 98, 0, 0, 1, 96, 98, 0, 0, 1, 97, 98, 0,
                0, 1, 98, 98, 0, 0, 1, 99, 98, 0, 0, 1, 100, 98, 0, 0, 1, 101,
                98, 0, 0, 1, 102, 98, 0, 0, 1, 103, 98, 0, 0, 1, 104, 98, 0, 0,
                1, 105, 98, 0, 0, 1, 106, 98, 0, 0, 1, 107, 98, 0, 0, 1, 108,
                98, 0, 0, 1, 109, 98, 0, 0, 1, 110, 98, 0, 0, 1, 111, 98, 0, 0,
                1, 112, 98, 0, 0, 1, 113, 98, 0, 0, 1, 114, 98, 0, 0, 1, 115,
                98, 0, 0, 1, 116, 98, 0, 0, 1, 117, 98, 0, 0, 1, 118, 98, 0, 0,
                1, 119, 98, 0, 0, 1, 120, 98, 0, 0, 1, 121, 98, 0, 0, 1, 122,
                98, 0, 0, 1, 123, 98, 0, 0, 1, 124, 98, 0, 0, 1, 125, 98, 0, 0,
                1, 126, 98, 0, 0, 1, 127, 98, 0, 0, 1, 128, 98, 0, 0, 1, 129,
                98, 0, 0, 1, 130, 98, 0, 0, 1, 131, 98, 0, 0, 1, 132, 98, 0, 0,
                1, 133, 98, 0, 0, 1, 134, 98, 0, 0, 1, 135, 98, 0, 0, 1, 136,
                98, 0, 0, 1, 137, 98, 0, 0, 1, 138, 98, 0, 0, 1, 139, 98, 0, 0,
                1, 140, 98, 0, 0, 1, 141, 98, 0, 0, 1, 142, 98, 0, 0, 1, 143);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding the nil/empty list.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeNil() throws IOException {
        Nil input = new Nil();
        byte[] expected = Utils.toBytes(131, 106);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding binary data.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeBinary() throws IOException {
        Binary input = new Binary(Utils.toBytes(97, 98, 99, 100));
        byte[] expected = Utils.toBytes(131, 109, 0, 0, 0, 4, 97, 98, 99, 100);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a string (which Erlang represents as a list of integers).
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeString() throws IOException {
        String input = "now get your ass to mars";
        byte[] expected = Utils.toBytes(131, 107, 0, 24, 110, 111, 119, 32,
                103, 101, 116, 32, 121, 111, 117, 114, 32, 97, 115, 115, 32,
                116, 111, 32, 109, 97, 114, 115);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a new reference.
     * 
     * @throws IOException
     */
    @Test
    @SuppressWarnings("serial")
    public void testEncodeNewReference() throws IOException {
        NewReference input = new NewReference(new Atom("nonode@nohost"),
                Byte.valueOf("0"), new ArrayList<Integer>() {
                    {
                        add(0);
                        add(0);
                        add(94);
                    }
                });
        byte[] expected = Utils.toBytes(131, 114, 0, 3, 115, 13, 110, 111, 110,
                111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 94);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding an Erlang function.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeErlFunction() throws IOException {
        ErlFunction input = new ErlFunction(new Pid(new Atom("nonode@nohost"),
                31, 0, Byte.valueOf("0")), new Atom("ilia_atom"),
                Byte.valueOf("1"), Byte.valueOf("1"), new ArrayList<Object>());
        byte[] expected = Utils.toBytes(131, 117, 0, 0, 0, 0, 103, 115, 13,
                110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
                0, 0, 0, 31, 0, 0, 0, 0, 0, 115, 9, 105, 108, 105, 97, 95, 97,
                116, 111, 109, 97, 1, 97, 1);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding an Erlang lambda created at runtime.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeNewFunction() throws IOException {
        NewFunction input = new NewFunction(Byte.valueOf("0"), new String(
                Utils.toBytes(184, 143, 129, 148, 32, 145, 63, 179, 91, 201,
                        150, 94, 151, 97, 58, 227)), 1, new Atom("erl_eval"),
                Byte.valueOf("20"), 67289768, new Pid(
                        new Atom("nonode@nohost"), 31, 0, Byte.valueOf("0")),
                new ArrayList<Object>());
        byte[] expected = Utils.toBytes(131, 112, 0, 0, 0, 71, 0, 184, 143,
                129, 148, 32, 145, 63, 179, 91, 201, 150, 94, 151, 97, 58, 227,
                0, 0, 0, 1, 0, 0, 0, 0, 115, 8, 101, 114, 108, 95, 101, 118,
                97, 108, 97, 20, 98, 4, 2, 194, 168, 103, 115, 13, 110, 111,
                110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0,
                31, 0, 0, 0, 0, 0);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding an Erlang export specification.
     * 
     * @throws IOException
     */
    @Test
    public void testEncodeExport() throws IOException {
        Export input = new Export(new Atom("erlang"), new Atom("self"),
                Byte.valueOf("0"));
        byte[] expected = Utils.toBytes(131, 113, 115, 6, 101, 114, 108, 97,
                110, 103, 115, 4, 115, 101, 108, 102, 97, 0);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a list of mixed data.
     * 
     * @throws IOException
     */
    @Test
    @SuppressWarnings("serial")
    public void testEncodeList() throws IOException {
        List<Object> input = new ArrayList<Object>() {
            {
                add((Byte) (byte) 69);
                add((Byte) (byte) 42);
                add("arnold");
            }
        };

        byte[] expected = Utils.toBytes(131, 108, 0, 0, 0, 3, 97, 69, 97, 42,
                107, 0, 6, 97, 114, 110, 111, 108, 100, 106);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }

    /**
     * Test encoding a map of data (should become a proplist).
     * 
     * @throws IOException
     */
    @Test
    @SuppressWarnings("serial")
    public void testEncodeMap() throws IOException {
        Map<Object, Object> input = new HashMap<Object, Object>() {
            {
                put("one", (Byte) (byte) 1);
                put("two", (Byte) (byte) 2);
                put("three", (Byte) (byte) 3);
            }
        };

        byte[] expected = Utils.toBytes(131, 108, 0, 0, 0, 3, 104, 2, 107, 0,
                3, 116, 119, 111, 97, 2, 104, 2, 107, 0, 3, 111, 110, 101, 97,
                1, 104, 2, 107, 0, 5, 116, 104, 114, 101, 101, 97, 3, 106);

        StreamEmulator stream = new StreamEmulator();
        Encoder.encode(input, stream);
        byte[] actual = stream.getBytes();

        assertArrayEquals(expected, actual);
    }
}
