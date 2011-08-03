package org.hurricane.driver.test;

import java.util.ArrayList;
import java.math.BigInteger;
import org.hurricane.driver.StreamEmulator;
import org.hurricane.driver.Decoder;
import org.hurricane.driver.datatypes.AtomCacheRef;
import org.hurricane.driver.datatypes.Atom;
import org.hurricane.driver.datatypes.Reference;
import org.hurricane.driver.datatypes.Port;
import org.hurricane.driver.datatypes.Pid;
import org.hurricane.driver.datatypes.Tuple;
import org.hurricane.driver.datatypes.Nil;
import org.hurricane.driver.datatypes.Binary;
import org.hurricane.driver.datatypes.NewFunction;
import org.hurricane.driver.datatypes.ErlFunction;
import org.hurricane.driver.datatypes.Export;
import org.hurricane.driver.datatypes.NewReference;
import org.hurricane.driver.datatypes.BitBinary;

public class Driver {
    static class EncodeTest {
        public int[] mBytes;
        public Object mExpected;

        public EncodeTest(int[] bytes, Object expected) {
            mBytes = bytes;
            mExpected = expected;
        }
    }

    static class Failure {
        public Object mExpected;
        public Object mActual;

        public Failure(Object expected, Object actual) {
            mExpected = expected;
            mActual = actual;
        }

        public String toString() {
            return "Expected: \n    " + mExpected + "\nActual: \n    " + mActual + "\n";
        }
    }

    public static byte toByte(int i) {
        return (byte) i;
    }

    public static byte[] toBytes(int[] is) {
        byte[] bs = new byte[is.length];
        for (int i = 0; i < is.length; i++) {
            bs[i] = toByte(is[i]);
        }
        return bs;
    }

    public static void main(String[] args) throws Exception {
        ArrayList<EncodeTest> encodeTests = new ArrayList<EncodeTest>();

        int[] atomCacheRefTestBytes = {131,82,1};
        encodeTests.add(
            new EncodeTest(
                atomCacheRefTestBytes,
                new AtomCacheRef(toByte(1))
            )
        );

        int[] smallIntegerTestBytes = {131,97,123};
        encodeTests.add(
            new EncodeTest(
                smallIntegerTestBytes,
                Byte.valueOf("123")
            )
        );

        int[] integerExtTestBytes = {131,98,0,0,4,210};
        encodeTests.add(
            new EncodeTest(
                integerExtTestBytes,
                new Integer(1234)
            )
        );

        int[] integerExtNegativeTestBytes = {131,98,255,255,255,187};
        encodeTests.add(
            new EncodeTest(
                integerExtNegativeTestBytes,
                new Integer(-69)
            )
        );

        int[] floatExtTestBytes = {
            131,99,49,46,49,48,48,48,48,48,48,48,48,48,48,48,48,
            48,48,48,56,56,56,50,101,43,48,48,0,0,0,0,0
        };
        encodeTests.add(
            new EncodeTest(
                floatExtTestBytes,
                new Double("1.1")
            )
        );

        int[] atomExtTestBytes = {
            131,100,0,9,105,108,105,97,95,97,116,111,109
        };
        encodeTests.add(
            new EncodeTest(
                atomExtTestBytes,
                new Atom("ilia_atom")
            )
        );

        int[] referenceExtTestBytes = {
            131,101,100,0,9,105,108,105,97,95,97,116,111,109,1,1,1,1,42
        };
        encodeTests.add(
            new EncodeTest(
                referenceExtTestBytes,
                new Reference(
                    new Atom("ilia_atom"), 16843009, Byte.valueOf("42")
                )
            )
        );

        int[] portExtTestBytes = {
            131,102,100,0,13,110,111,110,111,100,101,64,110,111,
            104,111,115,116,0,0,1,245,0
        };
        encodeTests.add(
            new EncodeTest(
                portExtTestBytes,
                new Port(
                    new Atom("nonode@nohost"), 501, Byte.valueOf("0")
                )
            )
        );

        int[] pidExtTestBytes = {
            131,103,100,0,13,110,111,110,111,100,101,64,110,111,
            104,111,115,116,0,0,0,31,0,0,0,0,0
        };
        encodeTests.add(
            new EncodeTest(
                pidExtTestBytes,
                new Pid(
                    new Atom("nonode@nohost"), 31, 0, Byte.valueOf("0")
                )
            )
        );

        int[] smallTupleExtTestBytes = {
            131,104,2,97,42,97,69
        };
        Tuple smallTupleTest = new Tuple();
        smallTupleTest.mElements.add(Byte.valueOf("42"));
        smallTupleTest.mElements.add(Byte.valueOf("69"));
        encodeTests.add(
            new EncodeTest(
                smallTupleExtTestBytes,
                smallTupleTest
            )
        );

        int[] largeTupleExtTestBytes = {
            131,105,0,0,1,26,97,42,97,69,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1, 
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1, 
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,
            97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,1,97,69
        };
        Tuple largeTupleTest = new Tuple();
        largeTupleTest.mElements.add(Byte.valueOf("42"));
        largeTupleTest.mElements.add(Byte.valueOf("69"));
        for (int i = 0; i < 279; i++) {
            largeTupleTest.mElements.add(Byte.valueOf("1"));
        }
        largeTupleTest.mElements.add(Byte.valueOf("69"));
        encodeTests.add(
            new EncodeTest(
                largeTupleExtTestBytes,
                largeTupleTest
            )
        );

        int[] nilExtTestBytes = {
            131,106
        };
        encodeTests.add(new EncodeTest(nilExtTestBytes, new Nil()));

        int[] stringExtTestBytes = {
            131,107,0,24,110,111,119,32,103,101,116,32,121,111,
            117,114,32,97,115,115,32,116,111,32,109,97,114,115
        };
        encodeTests.add(
            new EncodeTest(
                stringExtTestBytes,
                "now get your ass to mars"
            )
        );

        int[] nilTerminatedListExtTestBytes = {
            131,108,0,0,0,3,98,0,0,4,0,97,69,97,42,106
        };
        ArrayList<Object> nilTerminatedList = new ArrayList<Object>();
        nilTerminatedList.add(1024);
        nilTerminatedList.add(Byte.valueOf("69"));
        nilTerminatedList.add(Byte.valueOf("42"));
        encodeTests.add(
            new EncodeTest(
                nilTerminatedListExtTestBytes,
                nilTerminatedList
            )
        );

        int[] builderTerminatedListExtTestBytes = {
            131,108,0,0,0,3,98,0,0,4,0,97,69,97,42,97,1
        };
        ArrayList<Object> builderTerminatedList = new ArrayList<Object>();
        builderTerminatedList.add(1024);
        builderTerminatedList.add(Byte.valueOf("69"));
        builderTerminatedList.add(Byte.valueOf("42"));
        builderTerminatedList.add(Byte.valueOf("1"));
        encodeTests.add(
            new EncodeTest(
                builderTerminatedListExtTestBytes,
                builderTerminatedList
            )
        );

        int[] binaryExtTestBytes = {
            131,109,0,0,0,24,110,111,119,32,103,101,116,32,121,
            111,117,114,32,97,115,115,32,116,111,32,109,97,114,115
        };
        encodeTests.add(
            new EncodeTest(
                binaryExtTestBytes,
                new Binary(new String("now get your ass to mars").getBytes())
            )
        );

        int[] smallBigExtTestBytes = {
            131,110,5,0,5,228,183,122,4
        };
        encodeTests.add(
            new EncodeTest(
                smallBigExtTestBytes,
                new BigInteger("19238740997")
            )
        );

        int[] largeBigExtTestBytes = {
            131,111,0,0,1,2,0,77,166,39,216,70,78,172,93,52,181,124,101,170,
            246,159,16,153,154,36,214,242,85,167,60,82,247,27,245,116,125,
            142,21,240,221,108,192,132,152,51,154,161,28,103,117,214,153,173,
            30,239,99,160,203,22,156,85,168,87,248,230,10,115,56,202,134,245,
            68,118,184,171,238,51,180,1,81,139,164,161,186,231,229,45,235,
            244,167,92,107,159,27,27,195,22,23,236,229,112,138,40,150,70,107,
            175,46,27,67,83,124,247,18,87,144,167,18,212,14,129,63,190,119,
            149,68,165,43,152,211,55,3,54,145,143,121,37,88,178,78,178,210,
            187,209,95,180,82,43,15,124,42,219,102,36,195,96,61,131,32,224,3,
            92,102,31,168,163,134,238,181,13,36,71,28,218,181,142,191,71,241,
            91,166,118,178,91,243,2,40,76,251,97,47,34,78,233,253,137,228,
            231,176,73,165,147,17,247,147,24,66,100,211,64,134,14,184,130,
            153,249,87,2,246,140,79,220,174,26,78,214,64,224,88,139,150,122,
            184,197,160,31,203,192,9,112,34,66,156,248,131,70,190,215,162,230,
            162,74,181,184,36,105,229,7,168,75,47,80,83,2
        };
        encodeTests.add(
            new EncodeTest(
                largeBigExtTestBytes,
                new BigInteger(
                    "192387409991237409182730498172309487120398470129387401928374" +
                    "019283740918237409812730498560981723098461029384712093784098" +
                    "127364098712304987120394861023984701293561029387502983750921" +
                    "837409812734098123095620398571023948109274019238740912836750" +
                    "912837650981273054817230946120398571029386512983740591286350" +
                    "918276305918263059126305971260951623098712039857120936501293" +
                    "650912836750918263509281365091283650918263059812635091827365" +
                    "098172309012740129387409128374091283674091283764098127340981" +
                    "273049812734090238740918237409812730498172304987213094871230" +
                    "948712309487123098560129365095186320958612039586203956203985" +
                    "76019234870213977677"
                )
            )
        );

        int[] newFunctionExtTestBytes = {
            131,112,0,0,0,71,0,184,143,129,148,32,145,63,179,91,201,150,
            94,151,97,58,227,0,0,0,1,0,0,0,0,115,8,101,114,108,95,101,
            118,97,108,97,20,98,4,2,194,168,103,115,13,110,111,110,111,
            100,101,64,110,111,104,111,115,116,0,0,0,31,0,0,0,0,0,
        };
        int[] newFunctionUniqBytes = {
            184,143,129,148,32,145,63,179,91,201,150,94,151,97,58,227
        };
        encodeTests.add(
            new EncodeTest(
                newFunctionExtTestBytes,
                new NewFunction(
                    Byte.valueOf("0"), new String(toBytes(newFunctionUniqBytes)), 1,
                    new Atom("erl_eval"), Byte.valueOf("20"), 67289768,
                    new Pid(new Atom("nonode@nohost"), 31, 0, Byte.valueOf("0")),
                    new ArrayList<Object>()
                )
            )
        );

        int[] functionExtTestBytes = {
            131,117,0,0,0,0,103,100,0,13,110,111,110,111,100,101,64,110,
            111,104,111,115,116,0,0,0,31,0,0,0,0,0,100,0,9,105,108,105,
            97,95,97,116,111,109,97,1,97,1
        };
        encodeTests.add(
            new EncodeTest(
                functionExtTestBytes,
                new ErlFunction(
                    new Pid(
                        new Atom("nonode@nohost"), 31, 0, Byte.valueOf("0")
                    ),
                    new Atom("ilia_atom"), Byte.valueOf("1"),
                    Byte.valueOf("1"), new ArrayList<Object>()
                )
            )
        );

        int[] exportExtTestBytes = {
            131,113,100,0,6,101,114,108,97,110,103,100,0,4,115,101,108,
            102,97,0
        };
        encodeTests.add(
            new EncodeTest(
                exportExtTestBytes,
                new Export(
                    new Atom("erlang"), new Atom("self"), Byte.valueOf("0")
                )
            )
        );

        int[] newReferenceExtTestBytes = {
            131,114,0,3,100,0,13,110,111,110,111,100,101,64,110,111,104,
            111,115,116,0,0,0,0,94,0,0,0,0,0,0,0,0
        };
        ArrayList<Integer> newReferenceIds = new ArrayList<Integer>();
        newReferenceIds.add(0);
        newReferenceIds.add(0);
        newReferenceIds.add(94);
        encodeTests.add(
            new EncodeTest(
                newReferenceExtTestBytes,
                new NewReference(
                    new Atom("nonode@nohost"),
                    Byte.valueOf("0"),
                    newReferenceIds
                )
            )
        );

        int[] bitBinaryExtTestBytes = {131,77,0,0,0,4,4,195,139,30,64};
        int[] bitBinaryBytes = {195,139,30,64};
        encodeTests.add(
            new EncodeTest(
                bitBinaryExtTestBytes,
                new BitBinary(Byte.valueOf("4"), toBytes(bitBinaryBytes))
            )
        );
        int[] newFloatExtTestBytes = {131,70,1,1,1,1,1,1,1,1};
        encodeTests.add(
            new EncodeTest(
                newFloatExtTestBytes,
                new Double("8.321552E-317")
            )
        );

        StreamEmulator stream = new StreamEmulator();

        EncodeTest encodeTest;
        Object actual;
        ArrayList<Failure> failures = new ArrayList<Failure>();
        System.out.print("Encode: ");
        for (int i = 0; i < encodeTests.size(); i++) {
            encodeTest = encodeTests.get(i);
            stream.clear();
            stream.write(toBytes(encodeTest.mBytes));
            actual = Decoder.decode(stream);

            if (encodeTest.mExpected.equals(actual)) {
                System.out.print(".");
            } else {
                System.out.print("F");
                failures.add(new Failure(encodeTest.mExpected, actual));
            }
        }
        System.out.println();

        for (int i = 0; i < failures.size(); i++) {
            System.out.println(failures.get(i));
        }
    }
}
