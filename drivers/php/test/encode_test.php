<?php
require_once dirname(__FILE__) . '/../erl_codec.php';

/**
 * Tests that all encoding works correctly.
 */
class EncodeTest extends PHPUnit_Framework_TestCase
{
    /**
     * Test encoding a floating point number.
     *
     * @return void
     */
    public function testEncodeFloat()
    {
        $input = 1.1;
        $expected = array(
            131, 70, 63, 241, 153, 153, 153, 153, 153, 154
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a bit binary (space-compressed binary).
     *
     * @return void
     */
    public function testEncodeBitBinary()
    {
        $input = new Erlang\BitBinary(4, "\xc3\x8b\x1e@");
        $expected = array(131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an atom cache reference.
     *
     * @return void
     */
    public function testEncodeAtomCacheRef()
    {
        $input = new Erlang\AtomCacheRef(1);
        $expected = array(131, 82, 1);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a byte.
     *
     * @return void
     */
    public function testEncodeByte()
    {
        $input = 69;
        $expected = array(131, 97, 69);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a negative integer.
     *
     * @return void
     */
    public function testEncodeNegativeInteger()
    {
        $input = -69;
        $expected = array(131, 98, 255, 255, 255, 187);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a large number. PHP does not have a proper
     *
     * @return void
     * representation for big numbers, so they are converted to floating
     * point numbers.
     */
    public function testEncodeLargeNumber()
    {
        $input = pow(69, 69);
        $expected = array(131, 70, 90, 70, 113, 98, 55, 2, 224, 150);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a huge number. PHP does not have a proper
     *
     * @return void
     * representation for big numbers, so they are converted to floating
     * point numbers.
     */
    public function testEncodeHugeNumber()
    {
        $input = pow(-69, 696);
        $expected = array(131, 70, 127, 240, 0, 0, 0, 0, 0, 0);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an atom.
     *
     * @return void
     */
    public function testEncodeAtom()
    {
        $input = new Erlang\Atom('ilia_atom');
        $expected = array(
            131, 115, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a long atom.
     *
     * @return void
     */
    public function testEncodeLongAtom()
    {
        $input = new Erlang\Atom('ilia_atom_is_quite_a_long_atom');
        $expected = array(
            131, 100, 0, 30, 105, 108, 105, 97, 95, 97, 116, 111, 109,
            95, 105, 115, 95, 113, 117, 105, 116, 101, 95, 97, 95, 108,
            111, 110, 103, 95, 97, 116, 111, 109
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a reference.
     *
     * @return void
     */
    public function testEncodeReference()
    {
        $input = new Erlang\Reference(
            new Erlang\Atom('nonode@nohost'), 69, 0
        );
        $expected = array(
            131, 101, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 69, 0
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a port identifier.
     *
     * @return void
     */
    public function testEncodePort()
    {
        $input = new Erlang\Port(
            new Erlang\Atom('nonode@nohost'), 69, 0
        );
        $expected = array(
            131, 102, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 69, 0
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a process identifier.
     *
     * @return void
     */
    public function testEncodePid()
    {
        $input = new Erlang\Pid(
            new Erlang\Atom('nonode@nohost'), 69, 0, 0
        );
        $expected = array(
            131, 103, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 69, 0, 0, 0, 0, 0
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a small tuple.
     *
     * @return void
     */
    public function testEncodeSmallTuple()
    {
        $input = new Erlang\Tuple(range(0, 9));
        $expected = array(
            131, 104, 10, 97, 0, 97, 1, 97, 2, 97, 3, 97, 4, 97, 5, 97,
            6, 97, 7, 97, 8, 97, 9
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a large tuple.
     *
     * @return void
     */
    public function testEncodeLargeTuple()
    {
        $input = new Erlang\Tuple(range(0, 400));
        $expected = array(
            131, 105, 0, 0, 1, 145, 97, 0, 97, 1, 97, 2, 97, 3, 97, 4,
            97, 5, 97, 6, 97, 7, 97, 8, 97, 9, 97, 10, 97, 11, 97, 12,
            97, 13, 97, 14, 97, 15, 97, 16, 97, 17, 97, 18, 97, 19, 97,
            20, 97, 21, 97, 22, 97, 23, 97, 24, 97, 25, 97, 26, 97, 27,
            97, 28, 97, 29, 97, 30, 97, 31, 97, 32, 97, 33, 97, 34, 97,
            35, 97, 36, 97, 37, 97, 38, 97, 39, 97, 40, 97, 41, 97, 42,
            97, 43, 97, 44, 97, 45, 97, 46, 97, 47, 97, 48, 97, 49, 97,
            50, 97, 51, 97, 52, 97, 53, 97, 54, 97, 55, 97, 56, 97, 57,
            97, 58, 97, 59, 97, 60, 97, 61, 97, 62, 97, 63, 97, 64, 97,
            65, 97, 66, 97, 67, 97, 68, 97, 69, 97, 70, 97, 71, 97, 72,
            97, 73, 97, 74, 97, 75, 97, 76, 97, 77, 97, 78, 97, 79, 97,
            80, 97, 81, 97, 82, 97, 83, 97, 84, 97, 85, 97, 86, 97, 87,
            97, 88, 97, 89, 97, 90, 97, 91, 97, 92, 97, 93, 97, 94, 97,
            95, 97, 96, 97, 97, 97, 98, 97, 99, 97, 100, 97, 101, 97,
            102, 97, 103, 97, 104, 97, 105, 97, 106, 97, 107, 97, 108,
            97, 109, 97, 110, 97, 111, 97, 112, 97, 113, 97, 114, 97,
            115, 97, 116, 97, 117, 97, 118, 97, 119, 97, 120, 97, 121,
            97, 122, 97, 123, 97, 124, 97, 125, 97, 126, 97, 127, 97,
            128, 97, 129, 97, 130, 97, 131, 97, 132, 97, 133, 97, 134,
            97, 135, 97, 136, 97, 137, 97, 138, 97, 139, 97, 140, 97,
            141, 97, 142, 97, 143, 97, 144, 97, 145, 97, 146, 97, 147,
            97, 148, 97, 149, 97, 150, 97, 151, 97, 152, 97, 153, 97,
            154, 97, 155, 97, 156, 97, 157, 97, 158, 97, 159, 97, 160,
            97, 161, 97, 162, 97, 163, 97, 164, 97, 165, 97, 166, 97,
            167, 97, 168, 97, 169, 97, 170, 97, 171, 97, 172, 97, 173,
            97, 174, 97, 175, 97, 176, 97, 177, 97, 178, 97, 179, 97,
            180, 97, 181, 97, 182, 97, 183, 97, 184, 97, 185, 97, 186,
            97, 187, 97, 188, 97, 189, 97, 190, 97, 191, 97, 192, 97,
            193, 97, 194, 97, 195, 97, 196, 97, 197, 97, 198, 97, 199,
            97, 200, 97, 201, 97, 202, 97, 203, 97, 204, 97, 205, 97,
            206, 97, 207, 97, 208, 97, 209, 97, 210, 97, 211, 97, 212,
            97, 213, 97, 214, 97, 215, 97, 216, 97, 217, 97, 218, 97,
            219, 97, 220, 97, 221, 97, 222, 97, 223, 97, 224, 97, 225,
            97, 226, 97, 227, 97, 228, 97, 229, 97, 230, 97, 231, 97,
            232, 97, 233, 97, 234, 97, 235, 97, 236, 97, 237, 97, 238,
            97, 239, 97, 240, 97, 241, 97, 242, 97, 243, 97, 244, 97,
            245, 97, 246, 97, 247, 97, 248, 97, 249, 97, 250, 97, 251,
            97, 252, 97, 253, 97, 254, 97, 255, 98, 0, 0, 1, 0, 98, 0,
            0, 1, 1, 98, 0, 0, 1, 2, 98, 0, 0, 1, 3, 98, 0, 0, 1, 4, 98,
            0, 0, 1, 5, 98, 0, 0, 1, 6, 98, 0, 0, 1, 7, 98, 0, 0, 1, 8,
            98, 0, 0, 1, 9, 98, 0, 0, 1, 10, 98, 0, 0, 1, 11, 98, 0, 0,
            1, 12, 98, 0, 0, 1, 13, 98, 0, 0, 1, 14, 98, 0, 0, 1, 15,
            98, 0, 0, 1, 16, 98, 0, 0, 1, 17, 98, 0, 0, 1, 18, 98, 0, 0,
            1, 19, 98, 0, 0, 1, 20, 98, 0, 0, 1, 21, 98, 0, 0, 1, 22,
            98, 0, 0, 1, 23, 98, 0, 0, 1, 24, 98, 0, 0, 1, 25, 98, 0, 0,
            1, 26, 98, 0, 0, 1, 27, 98, 0, 0, 1, 28, 98, 0, 0, 1, 29,
            98, 0, 0, 1, 30, 98, 0, 0, 1, 31, 98, 0, 0, 1, 32, 98, 0, 0,
            1, 33, 98, 0, 0, 1, 34, 98, 0, 0, 1, 35, 98, 0, 0, 1, 36,
            98, 0, 0, 1, 37, 98, 0, 0, 1, 38, 98, 0, 0, 1, 39, 98, 0, 0,
            1, 40, 98, 0, 0, 1, 41, 98, 0, 0, 1, 42, 98, 0, 0, 1, 43,
            98, 0, 0, 1, 44, 98, 0, 0, 1, 45, 98, 0, 0, 1, 46, 98, 0, 0,
            1, 47, 98, 0, 0, 1, 48, 98, 0, 0, 1, 49, 98, 0, 0, 1, 50,
            98, 0, 0, 1, 51, 98, 0, 0, 1, 52, 98, 0, 0, 1, 53, 98, 0, 0,
            1, 54, 98, 0, 0, 1, 55, 98, 0, 0, 1, 56, 98, 0, 0, 1, 57,
            98, 0, 0, 1, 58, 98, 0, 0, 1, 59, 98, 0, 0, 1, 60, 98, 0, 0,
            1, 61, 98, 0, 0, 1, 62, 98, 0, 0, 1, 63, 98, 0, 0, 1, 64,
            98, 0, 0, 1, 65, 98, 0, 0, 1, 66, 98, 0, 0, 1, 67, 98, 0, 0,
            1, 68, 98, 0, 0, 1, 69, 98, 0, 0, 1, 70, 98, 0, 0, 1, 71,
            98, 0, 0, 1, 72, 98, 0, 0, 1, 73, 98, 0, 0, 1, 74, 98, 0, 0,
            1, 75, 98, 0, 0, 1, 76, 98, 0, 0, 1, 77, 98, 0, 0, 1, 78,
            98, 0, 0, 1, 79, 98, 0, 0, 1, 80, 98, 0, 0, 1, 81, 98, 0, 0,
            1, 82, 98, 0, 0, 1, 83, 98, 0, 0, 1, 84, 98, 0, 0, 1, 85,
            98, 0, 0, 1, 86, 98, 0, 0, 1, 87, 98, 0, 0, 1, 88, 98, 0, 0,
            1, 89, 98, 0, 0, 1, 90, 98, 0, 0, 1, 91, 98, 0, 0, 1, 92,
            98, 0, 0, 1, 93, 98, 0, 0, 1, 94, 98, 0, 0, 1, 95, 98, 0, 0,
            1, 96, 98, 0, 0, 1, 97, 98, 0, 0, 1, 98, 98, 0, 0, 1, 99,
            98, 0, 0, 1, 100, 98, 0, 0, 1, 101, 98, 0, 0, 1, 102, 98, 0,
            0, 1, 103, 98, 0, 0, 1, 104, 98, 0, 0, 1, 105, 98, 0, 0, 1,
            106, 98, 0, 0, 1, 107, 98, 0, 0, 1, 108, 98, 0, 0, 1, 109,
            98, 0, 0, 1, 110, 98, 0, 0, 1, 111, 98, 0, 0, 1, 112, 98, 0,
            0, 1, 113, 98, 0, 0, 1, 114, 98, 0, 0, 1, 115, 98, 0, 0, 1,
            116, 98, 0, 0, 1, 117, 98, 0, 0, 1, 118, 98, 0, 0, 1, 119,
            98, 0, 0, 1, 120, 98, 0, 0, 1, 121, 98, 0, 0, 1, 122, 98, 0,
            0, 1, 123, 98, 0, 0, 1, 124, 98, 0, 0, 1, 125, 98, 0, 0, 1,
            126, 98, 0, 0, 1, 127, 98, 0, 0, 1, 128, 98, 0, 0, 1, 129,
            98, 0, 0, 1, 130, 98, 0, 0, 1, 131, 98, 0, 0, 1, 132, 98, 0,
            0, 1, 133, 98, 0, 0, 1, 134, 98, 0, 0, 1, 135, 98, 0, 0, 1,
            136, 98, 0, 0, 1, 137, 98, 0, 0, 1, 138, 98, 0, 0, 1, 139,
            98, 0, 0, 1, 140, 98, 0, 0, 1, 141, 98, 0, 0, 1, 142, 98, 0,
            0, 1, 143, 98, 0, 0, 1, 144
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a nil/empty list.
     *
     * @return void
     */
    public function testEncodeNil()
    {
        $input = null;
        $expected = array(131, 106);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding binary data.
     *
     * @return void
     */
    public function testEncodeBinary()
    {
        $input = new Erlang\Binary('abcd');
        $expected = array(131, 109, 0, 0, 0, 4, 97, 98, 99, 100);

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a string.
     *
     * @return void
     */
    public function testEncodeString()
    {
        $input = 'now get your ass to mars';
        $expected = array(
            131, 107, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121,
            111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97,
            114, 115
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a list with mixed data types.
     *
     * @return void
     */
    public function testEncodeList()
    {
        $input = array(69, 42, 'arnold');
        $expected = array(
            131, 108, 0, 0, 0, 3, 97, 69, 97, 42, 107, 0, 6, 97, 114,
            110, 111, 108, 100, 106
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an associative array.
     *
     * @return void
     */
    public function testEncodeAssociativeArray()
    {
        $input = array('fail' => 0, 'win' => 1);
        $expected = array(
            131, 108, 0, 0, 0, 2, 104, 2, 107, 0, 4, 102, 97, 105, 108,
            97, 0, 104, 2, 107, 0, 3, 119, 105, 110, 97, 1, 106
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding a new reference.
     *
     * @return void
     */
    public function testEncodeNewReference()
    {
        $input = new Erlang\NewReference(
            new Erlang\Atom('nonode@nohost'), 0, array(0, 0, 94)
        );
        $expected = array(
            131, 114, 0, 3, 115, 13, 110, 111, 110, 111, 100, 101, 64,
            110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 94
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an Erlang function.
     *
     * @return void
     */
    public function testEncodeFunction()
    {
        $input = new Erlang\ErlFunction(
            new Erlang\Pid(new Erlang\Atom('nonode@nohost'), 31, 0, 0),
            new Erlang\Atom('ilia_atom'), 1, 1, null
        );
        $expected = array(
            131, 117, 0, 0, 0, 0, 103, 115, 13, 110, 111, 110, 111, 100,
            101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0,
            0, 0, 115, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109, 97,
            1, 97, 1
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an Erlang lambda created at run-time.
     *
     * @return void
     */
    public function testEncodeNewFunction()
    {
        $input = new Erlang\NewFunction(
            0, "\xb8\x8f\x81\x94 \x91?\xb3[\xc9\x96^\x97a:\xe3", 1,
            new Erlang\Atom('erl_eval'), 20, 67289768,
            new Erlang\Pid(new Erlang\Atom('nonode@nohost'), 31, 0, 0),
            array()
        );
        $expected = array(
            131, 112, 0, 0, 0, 71, 0, 184, 143, 129, 148, 32, 145, 63,
            179, 91, 201, 150, 94, 151, 97, 58, 227, 0, 0, 0, 1, 0, 0,
            0, 0, 115, 8, 101, 114, 108, 95, 101, 118, 97, 108, 97, 20,
            98, 4, 2, 194, 168, 103, 115, 13, 110, 111, 110, 111, 100,
            101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0,
            0, 0
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an export specifier.
     *
     * @return void
     */
    public function testEncodeExport()
    {
        $input = new Erlang\Export(
            new Erlang\Atom('erlang'), new Erlang\Atom('self'), 0
        );
        $expected = array(
            131, 113, 115, 6, 101, 114, 108, 97, 110, 103, 115, 4, 115,
            101, 108, 102, 97, 0
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }

    /**
     * Test encoding an associative array.
     *
     * @return void
     */
    public function testEncodeAssociativeArray2()
    {
        $input = array('one' => 1, 'two' => 2, 'three' => 3);
        $expected = array(
            131, 108, 0, 0, 0, 3, 104, 2, 107, 0, 3, 111, 110, 101, 97,
            1, 104, 2, 107, 0, 3, 116, 119, 111, 97, 2, 104, 2, 107, 0,
            5, 116, 104, 114, 101, 101, 97, 3, 106
        );

        $stream = new Erlang\StreamEmulator();
        Erlang\encode($input, $stream);
        $actual = Erlang\from_binary($stream->data);

        $this->assertEquals($expected, $actual);
    }
}
