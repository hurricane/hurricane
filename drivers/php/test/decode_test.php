<?php
require_once dirname(__FILE__) . '/../erl_codec.php';

/**
 * Tests that all decoding works correctly.
 */
class DecodeTest extends PHPUnit_Framework_TestCase
{
    /**
     * Test decoding an atom cache reference.
     *
     * @return void
     */
    public function testDecodeAtomCacheRef()
    {
        $input = array(131, 82, 1);
        $expected = new Erlang\AtomCacheRef(1);

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a byte.
     *
     * @return void
     */
    public function testDecodeByte()
    {
        $input = array(131, 97, 123);
        $expected = 123;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding an integer.
     *
     * @return void
     */
    public function testDecodeInteger()
    {
        $input = array(131, 98, 0, 0, 4, 210);
        $expected = 1234;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a negative integer.
     *
     * @return void
     */
    public function testDecodeNegativeInteger()
    {
        $input = array(131, 98, 255, 255, 255, 187);
        $expected = -69;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a floating point number represented as a string.
     *
     * @return void
     */
    public function testDecodeFloat()
    {
        $input = array(
            131, 99, 49, 46, 49, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
            48, 48, 48, 48, 48, 56, 56, 56, 50, 101, 43, 48, 48, 0, 0,
            0, 0, 0
        );
        $expected = 1.1;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding an atom.
     *
     * @return void
     */
    public function testDecodeAtom()
    {
        $input = array(
            131, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109
        );
        $expected = new Erlang\Atom('ilia_atom');

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a reference.
     *
     * @return void
     */
    public function testDecodeReference()
    {
        $input = array(
            131, 101, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111,
            109, 1, 1, 1, 1, 42
        );
        $expected = new Erlang\Reference(
            new Erlang\Atom('ilia_atom'), 16843009, 42
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a port identifier.
     *
     * @return void
     */
    public function testDecodePort()
    {
        $input = array(
            131, 102, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 1, 245, 0
        );
        $expected = new Erlang\Port(
            new Erlang\Atom('nonode@nohost'), 501, 0
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a process identifier.
     *
     * @return void
     */
    public function testDecodePid()
    {
        $input = array(
            131, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0
        );
        $expected = new Erlang\Pid(
            new Erlang\Atom('nonode@nohost'), 31, 0, 0
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a small tuple.
     *
     * @return void
     */
    public function testDecodeSmallTuple()
    {
        $input = array(131, 104, 2, 97, 42, 97, 69);
        $expected = new Erlang\Tuple(array(42, 69));

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a large tuple.
     *
     * @return void
     */
    public function testDecodeLargeTuple()
    {
        $input = array(
            131, 105, 0, 0, 1, 26, 97, 42, 97, 69, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
            1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
            97, 1, 97, 1, 97, 1, 97, 1, 97, 69
        );
        $expected = new Erlang\Tuple(array(
            42, 69, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 69
        ));

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a nil/empty list.
     *
     * @return void
     */
    public function testDecodeNil()
    {
        $input = array(131, 106);
        $expected = null;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a string.
     *
     * @return void
     */
    public function testDecodeString()
    {
        $input = array(
            131, 107, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121,
            111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97,
            114, 115
        );
        $expected = 'now get your ass to mars';

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a list.
     *
     * @return void
     */
    public function testDecodeList()
    {
        $input = array(
            131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97, 69, 97, 42, 106
        );
        $expected = array(1024, 69, 42);

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a list with a trailer item.
     *
     * @return void
     */
    public function testDecodeListTrailer()
    {
        $input = array(
            131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97, 69, 97, 42, 97, 1
        );
        $expected = array(1024, 69, 42, 1);

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a binary.
     *
     * @return void
     */
    public function testDecodeBinary()
    {
        $input = array(
            131, 109, 0, 0, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32,
            121, 111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109,
            97, 114, 115, 
        );
        $expected = new Erlang\Binary('now get your ass to mars');

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding an integer in the "small big ext" format.
     *
     * @return void
     */
    public function testDecodeLargeInteger()
    {
        $input = array(131, 110, 5, 0, 5, 228, 183, 122, 4);
        $expected = 19238740997;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding an integer in the "large big ext" format.
     *
     * @return void
     */
    public function testDecodeHugeInteger()
    {
        $input = array(131, 111, 0, 0, 0, 1, 0, 77);
        $expected = 77;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a new reference.
     *
     * @return void
     */
    public function testDecodeNewReference()
    {
        $input = array(
            131, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101,
            64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 94, 0, 0, 0,
            0, 0, 0, 0, 0
        );
        $expected = new Erlang\NewReference(
            new Erlang\Atom('nonode@nohost'), 0, array(0, 0, 94)
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a small atom.
     *
     * @return void
     */
    public function testDecodeSmallAtom()
    {
        $input = array(131, 115, 4, 97, 98, 99, 100);
        $expected = new Erlang\Atom('abcd');

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding an export specifier.
     *
     * @return void
     */
    public function testDecodeExport()
    {
        $input = array(
            131, 113, 100, 0, 6, 101, 114, 108, 97, 110, 103, 100, 0, 4,
            115, 101, 108, 102, 97, 0
        );
        $expected = new Erlang\Export(
            new Erlang\Atom('erlang'), new Erlang\Atom('self'), 0
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a lambda created at run-time.
     *
     * @return void
     */
    public function testDecodeNewFunction()
    {
        $input = array(
            131, 112, 0, 0, 0, 71, 0, 184, 143, 129, 148, 32, 145, 63,
            179, 91, 201, 150, 94, 151, 97, 58, 227, 0, 0, 0, 1, 0, 0,
            0, 0, 115, 8, 101, 114, 108, 95, 101, 118, 97, 108, 97, 20,
            98, 4, 2, 194, 168, 103, 115, 13, 110, 111, 110, 111, 100,
            101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0,
            0, 0
        );
        $expected = new Erlang\NewFunction(
            0, "\xb8\x8f\x81\x94 \x91?\xb3[\xc9\x96^\x97a:\xe3", 1,
            new Erlang\Atom('erl_eval'), 20, 67289768,
            new Erlang\Pid(new Erlang\Atom('nonode@nohost'), 31, 0, 0),
            null
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding an Erlang function.
     *
     * @return void
     */
    public function testDecodeFunction()
    {
        $input = array(
            131, 117, 0, 0, 0, 0, 103, 100, 0, 13, 110, 111, 110, 111,
            100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0,
            0, 0, 0, 0, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111,
            109, 97, 1, 97, 1
        );
        $expected = new Erlang\ErlFunction(
            new Erlang\Pid(
                new Erlang\Atom('nonode@nohost'), 31, 0, 0
            ),
            new Erlang\Atom('ilia_atom'), 1, 1, null
        );

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a bit binary (space-compressed binary).
     *
     * @return void
     */
    public function testDecodeBitBinary()
    {
        $input = array(131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64);
        $expected = new Erlang\BitBinary(4, "\xc3\x8b\x1e@");

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test decoding a floating point number in the IEEE format.
     *
     * @return void
     */
    public function testDecodeNewFloat()
    {
        $input = array(131, 70, 1, 1, 1, 1, 1, 1, 1, 1);
        $expected = 7.7486041854893479e-304;

        $stream = new Erlang\StreamEmulator($input);
        $actual = Erlang\decode($stream);

        $this->assertEquals($expected, $actual);
    }
}
