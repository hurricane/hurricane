"""Unit tests for the Erlang encode/decode logic."""

import erl_codec
from unittest import TestCase
from cStringIO import StringIO


def stringio_from_bytes(byte_list):
    """Turn a list of bytes into a StringIO object."""
    return StringIO(''.join(chr(x) for x in byte_list))


def byte_list_from_stringio(io_obj):
    """Turn a StringIO object into a list of bytes."""
    return [ord(x) for x in io_obj.getvalue()]


class TestErlangCodec(TestCase):
    """Contains all test cases for encoding/decoding."""

    def test_decode_atom_cache_ref(self):
        """Test decoding an atom cache reference."""
        expected = erl_codec.AtomCacheRef(1)
        input_value = [131, 82, 1]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_small_integer(self):
        """Test decoding a small integer."""
        expected = 123
        input_value = [131, 97, 123]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_integer(self):
        """Test decoding a related integer."""
        expected = 1234
        input_value = [131, 98, 0, 0, 4, 210]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_negative_integer(self):
        """Test decoding a negative integer."""
        expected = -69
        input_value = [131, 98, 255, 255, 255, 187]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_float(self):
        """Test decoding a floating point number."""
        expected = 1.1
        input_value = [
            131, 99, 49, 46, 49, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
            48, 48, 48, 48, 48, 56, 56, 56, 50, 101, 43, 48, 48, 0, 0,
            0, 0, 0]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_atom(self):
        """Test decoding an atom."""
        expected = erl_codec.Atom('ilia_atom')
        input_value = [
            131, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_reference(self):
        """Test decoding an Erlang reference."""
        expected = erl_codec.Reference(
            erl_codec.Atom('ilia_atom'), 16843009, 42)
        input_value = [
            131, 101, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111,
            109, 1, 1, 1, 1, 42]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_port(self):
        """Test decoding an Erlang port."""
        expected = erl_codec.Port(
            erl_codec.Atom('nonode@nohost'), 501, 0)
        input_value = [
            131, 102, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 1, 245, 0]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_pid(self):
        """Test decoding an Erlang process identifier."""
        expected = erl_codec.Pid(
            erl_codec.Atom('nonode@nohost'), 31, 0, 0)
        input_value = [
            131, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_small_tuple(self):
        """Test decoding a small tuple."""
        expected = (42, 69)
        input_value = [131, 104, 2, 97, 42, 97, 69]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_large_tuple(self):
        """Test decoding a large tuple."""
        expected = (
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
            1, 1, 69)
        input_value = [
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
            97, 1, 97, 1, 97, 1, 97, 1, 97, 69]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_nil(self):
        """Test decoding a nil/empty list."""
        expected = None
        input_value = [131, 106]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_string(self):
        """Test decoding a string (list of integers)."""
        expected = 'now get your ass to mars'
        input_value = [
            131, 107, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121,
            111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97,
            114, 115]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_small_list(self):
        """Test decoding a small list of integers."""
        expected = [1024, 69, 42]
        input_value = [
            131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97, 69, 97, 42, 106]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_small_list_trail(self):
        """
        Test decoding a small list of integers with a trailer on the end
        of the list.
        """
        expected = [1024, 69, 42, 1]
        input_value = [
            131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97, 69, 97, 42,
            97, 1]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_binary(self):
        """Test decoding a binary value."""
        expected = erl_codec.Binary('now get your ass to mars')
        input_value = [
            131, 109, 0, 0, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32,
            121, 111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109,
            97, 114, 115]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_long(self):
        """Test decoding a long integer."""
        expected = 19238740997
        input_value = [131, 110, 5, 0, 5, 228, 183, 122, 4]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_huge_long(self):
        """Test decoding a very long integer."""
        expected = int(
            '1923874099912374091827304981723094871203984701293874019283'
            '7401928374091823740981273049856098172309846102938471209378'
            '4098127364098712304987120394861023984701293561029387502983'
            '7509218374098127340981230956203985710239481092740192387409'
            '1283675091283765098127305481723094612039857102938651298374'
            '0591286350918276305918263059126305971260951623098712039857'
            '1209365012936509128367509182635092813650912836509182630598'
            '1263509182736509817230901274012938740912837409128367409128'
            '3764098127340981273049812734090238740918237409812730498172'
            '3049872130948712309487123094871230985601293650951863209586'
            '1203958620395620398576019234870213977677')
        input_value = [
            131, 111, 0, 0, 1, 2, 0, 77, 166, 39, 216, 70, 78, 172, 93,
            52, 181, 124, 101, 170, 246, 159, 16, 153, 154, 36, 214,
            242, 85, 167, 60, 82, 247, 27, 245, 116, 125, 142, 21, 240,
            221, 108, 192, 132, 152, 51, 154, 161, 28, 103, 117, 214,
            153, 173, 30, 239, 99, 160, 203, 22, 156, 85, 168, 87, 248,
            230, 10, 115, 56, 202, 134, 245, 68, 118, 184, 171, 238, 51,
            180, 1, 81, 139, 164, 161, 186, 231, 229, 45, 235, 244, 167,
            92, 107, 159, 27, 27, 195, 22, 23, 236, 229, 112, 138, 40,
            150, 70, 107, 175, 46, 27, 67, 83, 124, 247, 18, 87, 144,
            167, 18, 212, 14, 129, 63, 190, 119, 149, 68, 165, 43, 152,
            211, 55, 3, 54, 145, 143, 121, 37, 88, 178, 78, 178, 210,
            187, 209, 95, 180, 82, 43, 15, 124, 42, 219, 102, 36, 195,
            96, 61, 131, 32, 224, 3, 92, 102, 31, 168, 163, 134, 238,
            181, 13, 36, 71, 28, 218, 181, 142, 191, 71, 241, 91, 166,
            118, 178, 91, 243, 2, 40, 76, 251, 97, 47, 34, 78, 233, 253,
            137, 228, 231, 176, 73, 165, 147, 17, 247, 147, 24, 66, 100,
            211, 64, 134, 14, 184, 130, 153, 249, 87, 2, 246, 140, 79,
            220, 174, 26, 78, 214, 64, 224, 88, 139, 150, 122, 184, 197,
            160, 31, 203, 192, 9, 112, 34, 66, 156, 248, 131, 70, 190,
            215, 162, 230, 162, 74, 181, 184, 36, 105, 229, 7, 168, 75,
            47, 80, 83, 2]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_new_reference(self):
        """Test decoding a new reference."""
        expected = erl_codec.NewReference(
            erl_codec.Atom('nonode@nohost'), 0, [0, 0, 94])
        input_value = [
            131, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101,
            64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 94, 0, 0, 0,
            0, 0, 0, 0, 0]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_short_atom(self):
        """Test decoding an atom with a short identifier."""
        expected = erl_codec.Atom('abcd')
        input_value = [131, 115, 4, 97, 98, 99, 100]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_new_function(self):
        """Test decoding an Erlang lambda made at run time."""
        expected = erl_codec.NewFunction(
            0, '\xb8\x8f\x81\x94 \x91?\xb3[\xc9\x96^\x97a:\xe3', 1,
            erl_codec.Atom('erl_eval'), 20, 67289768,
            erl_codec.Pid(erl_codec.Atom('nonode@nohost'), 31, 0, 0),
            [
                None,
                (
                    erl_codec.Atom('value'),
                    erl_codec.NewFunction(
                        2, '\xa3\xcf\xdc\xae\xca\x81\xee'
                        'r\xef\xe5\x03\x85Y34\xb9',
                        7, erl_codec.Atom('shell'), 7, 37281544,
                        erl_codec.Pid(
                            erl_codec.Atom('nonode@nohost'),
                            31, 0, 0
                        ),
                        [
                            erl_codec.Pid(
                                erl_codec.Atom('nonode@nohost'),
                                25, 0, 0
                            )
                        ]
                    )
                ),
                (
                    erl_codec.Atom('eval'),
                    erl_codec.NewFunction(
                        3, '\xa3\xcf\xdc\xae\xca\x81\xee'
                        'r\xef\xe5\x03\x85Y34\xb9', 14,
                        erl_codec.Atom('shell'), 24, 85590193,
                        erl_codec.Pid(
                            erl_codec.Atom('nonode@nohost'), 31, 0, 0
                        ),
                        [
                            (
                                erl_codec.Atom('value'),
                                erl_codec.NewFunction(
                                    2, '\xa3\xcf\xdc\xae\xca\x81\xee'
                                    'r\xef\xe5\x03\x85Y34\xb9', 7,
                                    erl_codec.Atom('shell'), 7,
                                    37281544,
                                    erl_codec.Pid(
                                        erl_codec.Atom('nonode@nohost'),
                                        31, 0, 0
                                    ),
                                    [
                                        erl_codec.Pid(
                                            erl_codec.Atom(
                                                'nonode@nohost'
                                            ),
                                            25, 0, 0
                                        )
                                    ]
                                )
                            ),
                            8207,
                            erl_codec.NewFunction(
                                1, '\xa3\xcf\xdc\xae\xca\x81\xee'
                                'r\xef\xe5\x03\x85Y34\xb9', 15,
                                erl_codec.Atom('shell'), 14, 53064318,
                                erl_codec.Pid(
                                    erl_codec.Atom('nonode@nohost'),
                                    31, 0, 0
                                ),
                                [
                                    (
                                        erl_codec.Atom('value'),
                                        erl_codec.NewFunction(
                                            2,
                                            '\xa3\xcf\xdc\xae\xca\x81'
                                            '\xeer\xef\xe5\x03\x85Y34'
                                            '\xb9',
                                            7, erl_codec.Atom('shell'),
                                            7, 37281544,
                                            erl_codec.Pid(
                                                erl_codec.Atom(
                                                    'nonode@nohost'
                                                ),
                                                31, 0, 0
                                            ),
                                            [
                                                erl_codec.Pid(
                                                    erl_codec.Atom(
                                                        'nonode@nohost'
                                                    ),
                                                    25, 0, 0
                                                )
                                            ]
                                        )
                                    ),
                                    8207,
                                    erl_codec.Pid(
                                        erl_codec.Atom('nonode@nohost'),
                                        25, 0, 0
                                    )
                                ]
                            ),
                            erl_codec.Pid(
                                erl_codec.Atom('nonode@nohost'), 25, 0, 0
                            )
                        ]
                    )
                ),
                [
                    (
                        erl_codec.Atom('clause'), 1, None, None,
                        [
                            (
                                erl_codec.Atom('atom'), 1,
                                erl_codec.Atom('ok')
                            )
                        ]
                    )
                ]
            ]
        )
        input_value = [
            131, 112, 0, 0, 2, 139, 0, 184, 143, 129, 148, 32, 145, 63,
            179, 91, 201, 150, 94, 151, 97, 58, 227, 0, 0, 0, 1, 0, 0,
            0, 4, 100, 0, 8, 101, 114, 108, 95, 101, 118, 97, 108, 97,
            20, 98, 4, 2, 194, 168, 103, 100, 0, 13, 110, 111, 110, 111,
            100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0,
            0, 0, 0, 0, 106, 104, 2, 100, 0, 5, 118, 97, 108, 117, 101,
            112, 0, 0, 0, 96, 2, 163, 207, 220, 174, 202, 129, 238, 114,
            239, 229, 3, 133, 89, 51, 52, 185, 0, 0, 0, 7, 0, 0, 0, 1,
            100, 0, 5, 115, 104, 101, 108, 108, 97, 7, 98, 2, 56, 223,
            8, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0, 103,
            100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104,
            111, 115, 116, 0, 0, 0, 25, 0, 0, 0, 0, 0, 104, 2, 100, 0,
            4, 101, 118, 97, 108, 112, 0, 0, 1, 161, 3, 163, 207, 220,
            174, 202, 129, 238, 114, 239, 229, 3, 133, 89, 51, 52, 185,
            0, 0, 0, 14, 0, 0, 0, 4, 100, 0, 5, 115, 104, 101, 108, 108,
            97, 24, 98, 5, 26, 0, 177, 103, 100, 0, 13, 110, 111, 110,
            111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0,
            31, 0, 0, 0, 0, 0, 104, 2, 100, 0, 5, 118, 97, 108, 117,
            101, 112, 0, 0, 0, 96, 2, 163, 207, 220, 174, 202, 129, 238,
            114, 239, 229, 3, 133, 89, 51, 52, 185, 0, 0, 0, 7, 0, 0, 0,
            1, 100, 0, 5, 115, 104, 101, 108, 108, 97, 7, 98, 2, 56,
            223, 8, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64,
            110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0,
            103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
            104, 111, 115, 116, 0, 0, 0, 25, 0, 0, 0, 0, 0, 98, 0, 0,
            32, 15, 112, 0, 0, 0, 208, 1, 163, 207, 220, 174, 202, 129,
            238, 114, 239, 229, 3, 133, 89, 51, 52, 185, 0, 0, 0, 15, 0,
            0, 0, 3, 100, 0, 5, 115, 104, 101, 108, 108, 97, 14, 98, 3,
            41, 178, 126, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101,
            64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0,
            0, 104, 2, 100, 0, 5, 118, 97, 108, 117, 101, 112, 0, 0, 0,
            96, 2, 163, 207, 220, 174, 202, 129, 238, 114, 239, 229, 3,
            133, 89, 51, 52, 185, 0, 0, 0, 7, 0, 0, 0, 1, 100, 0, 5,
            115, 104, 101, 108, 108, 97, 7, 98, 2, 56, 223, 8, 103, 100,
            0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111,
            115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0, 103, 100, 0, 13, 110,
            111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
            0, 0, 0, 25, 0, 0, 0, 0, 0, 98, 0, 0, 32, 15, 103, 100, 0,
            13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111,
            115, 116, 0, 0, 0, 25, 0, 0, 0, 0, 0, 103, 100, 0, 13, 110,
            111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
            0, 0, 0, 25, 0, 0, 0, 0, 0, 108, 0, 0, 0, 1, 104, 5, 100, 0,
            6, 99, 108, 97, 117, 115, 101, 97, 1, 106, 106, 108, 0, 0,
            0, 1, 104, 3, 100, 0, 4, 97, 116, 111, 109, 97, 1, 100, 0,
            2, 111, 107, 106, 106]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_export(self):
        """Test decoding an export specifier."""
        expected = erl_codec.Export(
            erl_codec.Atom('erlang'), erl_codec.Atom('self'), 0)
        input_value = [
            131, 113, 100, 0, 6, 101, 114, 108, 97, 110, 103, 100, 0, 4,
            115, 101, 108, 102, 97, 0]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_function(self):
        """Test decoding an Erlang function."""
        expected = erl_codec.Function(
            erl_codec.Pid(
                erl_codec.Atom('nonode@nohost'), 31, 0, 0),
                erl_codec.Atom('ilia_atom'), 1, 1, None)
        input_value = [
            131, 117, 0, 0, 0, 0, 103, 100, 0, 13, 110, 111, 110, 111,
            100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0,
            0, 0, 0, 0, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111,
            109, 97, 1, 97, 1]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_bit_binary(self):
        """Test decoding a bit binary (space-compressed binary)."""
        expected = erl_codec.BitBinary(4, '\xc3\x8b\x1e@')
        input_value = [131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_decode_new_float(self):
        """Test decoding a float in the IEEE format."""
        expected = 7.7486041854893479e-304
        input_value = [131, 70, 1, 1, 1, 1, 1, 1, 1, 1]

        stream = stringio_from_bytes(input_value)
        actual = erl_codec.decode(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_float(self):
        """Test encoding a floating point number to IEEE form."""
        expected = [131, 70, 63, 241, 153, 153, 153, 153, 153, 154]
        input_value = 1.1

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_bit_binary(self):
        """Test encoding a bit binary (space compressed binary)."""
        expected = [131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64]
        input_value = erl_codec.BitBinary(4, '\xc3\x8b\x1e@')

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_atom_cache_ref(self):
        """Test encoding an atom cache reference."""
        expected = [131, 82, 1]
        input_value = erl_codec.AtomCacheRef(1)

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_integer(self):
        """Test encoding an integer."""
        expected = [131, 97, 69]
        input_value = 69

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_negative_integer(self):
        """Test encoding a negative integer."""
        expected = [131, 98, 255, 255, 255, 187]
        input_value = -69

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_long_integer(self):
        """Test encoding a long integer."""
        expected = [131, 110, 8, 0, 254, 255, 179, 206, 71, 38, 88, 103]
        input_value = 7446744073709551614L

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_longer_integer(self):
        """Test encoding an even longer integer."""
        expected = [
            131, 110, 53, 0, 117, 116, 129, 23, 136, 197, 28, 142, 23,
            72, 229, 165, 191, 192, 22, 188, 155, 206, 43, 113, 63, 242,
            3, 25, 228, 209, 17, 66, 83, 52, 125, 56, 245, 58, 162, 52,
            201, 215, 74, 188, 3, 203, 142, 87, 67, 19, 43, 193, 5, 110,
            196, 226, 44]
        input_value = 69 ** 69

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_very_long_integer(self):
        """Test encoding a very long integer."""
        expected = [
            131, 111, 0, 0, 2, 20, 1, 33, 89, 30, 229, 196, 107, 139,
            7, 211, 176, 94, 210, 240, 73, 193, 252, 228, 16, 46, 39,
            41, 172, 134, 15, 148, 175, 202, 174, 39, 93, 95, 64, 112,
            61, 184, 30, 213, 51, 0, 148, 67, 252, 76, 57, 30, 101, 112,
            72, 159, 222, 69, 12, 72, 158, 21, 174, 51, 215, 253, 188,
            108, 235, 36, 137, 63, 135, 237, 38, 12, 135, 171, 231, 225,
            245, 246, 107, 70, 145, 37, 35, 181, 217, 230, 237, 151,
            244, 238, 71, 140, 158, 2, 236, 2, 93, 139, 192, 182, 2, 50,
            113, 43, 5, 236, 64, 243, 200, 20, 89, 177, 118, 93, 105,
            30, 252, 155, 221, 247, 170, 178, 142, 230, 55, 167, 20, 34,
            188, 215, 141, 245, 9, 222, 240, 100, 209, 43, 38, 114, 13,
            194, 226, 173, 132, 73, 15, 237, 138, 30, 190, 247, 101, 97,
            157, 20, 252, 135, 90, 24, 201, 203, 85, 23, 246, 227, 235,
            87, 138, 230, 66, 48, 114, 50, 107, 197, 216, 154, 225, 22,
            25, 242, 197, 212, 131, 185, 132, 162, 210, 65, 232, 14,
            213, 216, 36, 126, 216, 164, 16, 1, 74, 252, 199, 36, 155,
            160, 252, 165, 218, 85, 222, 27, 180, 119, 88, 156, 92, 20,
            184, 163, 209, 123, 104, 43, 183, 108, 101, 228, 132, 16,
            35, 210, 22, 245, 68, 110, 45, 82, 22, 142, 52, 122, 7, 105,
            14, 206, 113, 124, 70, 167, 77, 225, 59, 253, 180, 181, 216,
            137, 245, 9, 5, 80, 206, 16, 12, 185, 153, 155, 248, 201,
            114, 96, 179, 184, 11, 186, 233, 212, 121, 204, 235, 191,
            214, 163, 41, 83, 252, 84, 84, 56, 55, 60, 119, 101, 99,
            167, 129, 41, 23, 111, 107, 225, 103, 79, 81, 142, 19, 236,
            207, 115, 250, 103, 206, 126, 5, 43, 201, 60, 114, 237, 13,
            111, 210, 211, 204, 247, 189, 195, 138, 2, 41, 139, 167,
            180, 56, 59, 180, 225, 93, 12, 69, 20, 144, 251, 15, 59,
            155, 248, 59, 191, 181, 1, 221, 181, 69, 62, 218, 157, 173,
            43, 160, 213, 22, 155, 15, 1, 173, 97, 115, 172, 243, 209,
            234, 152, 42, 29, 161, 150, 134, 41, 197, 232, 51, 208, 97,
            230, 236, 83, 240, 74, 171, 221, 6, 125, 70, 51, 42, 185,
            243, 237, 232, 105, 145, 114, 181, 138, 130, 0, 139, 216,
            10, 137, 96, 156, 133, 150, 212, 79, 213, 5, 170, 161, 40,
            80, 163, 135, 65, 51, 2, 176, 182, 10, 25, 239, 40, 22, 192,
            166, 213, 250, 238, 175, 244, 201, 53, 0, 63, 196, 29, 36,
            59, 159, 202, 175, 18, 85, 170, 114, 224, 209, 94, 172, 65,
            182, 244, 181, 166, 29, 177, 235, 21, 228, 10, 247, 170,
            185, 42, 151, 195, 33, 255, 77, 5, 166, 98, 50, 4, 20, 44,
            227, 203, 198, 212, 3, 112, 48, 102, 20, 142, 238, 92, 183,
            111, 39, 7, 81, 72, 166, 102, 182, 15, 46, 10, 74, 211, 137,
            20, 168, 211, 1, 105, 1, 180, 151, 185, 129, 59, 77, 71, 42,
            88, 31, 94, 147, 11]
        input_value = -69 ** 696

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_atom(self):
        """Test encoding an atom."""
        expected = [
            131, 115, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109]
        input_value = erl_codec.Atom('ilia_atom')

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_long_atom(self):
        """Test encoding a long atom."""
        expected = [
            131, 100, 0, 30, 105, 108, 105, 97, 95, 97, 116, 111, 109,
            95, 105, 115, 95, 113, 117, 105, 116, 101, 95, 97, 95, 108,
            111, 110, 103, 95, 97, 116, 111, 109]
        input_value = erl_codec.Atom('ilia_atom_is_quite_a_long_atom')

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_reference(self):
        """Test encoding a reference."""
        expected = [
            131, 101, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 69, 0]
        input_value = erl_codec.Reference(
            erl_codec.Atom('nonode@nohost'), 69, 0)

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_port(self):
        """Test encoding a port identifier."""
        expected = [
            131, 102, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 69, 0]
        input_value = erl_codec.Port(
            erl_codec.Atom('nonode@nohost'), 69, 0)

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_pid(self):
        """Test encoding a process identifier."""
        expected = [
            131, 103, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
            111, 104, 111, 115, 116, 0, 0, 0, 69, 0, 0, 0, 0, 0]
        input_value = erl_codec.Pid(
            erl_codec.Atom('nonode@nohost'), 69, 0, 0)

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_small_tuple(self):
        """Test encoding a small tuple."""
        expected = [
            131, 104, 10, 97, 0, 97, 1, 97, 2, 97, 3, 97, 4, 97, 5, 97,
            6, 97, 7, 97, 8, 97, 9]
        input_value = tuple(range(10))

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_large_tuple(self):
        """Test encoding a large tuple."""
        expected = [
            131, 105, 0, 0, 1, 144, 97, 0, 97, 1, 97, 2, 97, 3, 97, 4,
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
            0, 1, 143]
        input_value = tuple(range(400))

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_nil(self):
        """Test encoding a nil/empty list."""
        expected = [131, 106]
        input_value = None

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_binary(self):
        """Test encoding binary data."""
        expected = [131, 109, 0, 0, 0, 4, 97, 98, 99, 100]
        input_value = erl_codec.Binary('abcd')

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_string(self):
        """Test encoding a string."""
        expected = [
            131, 107, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121,
            111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97,
            114, 115]
        input_value = 'now get your ass to mars'

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_list(self):
        """Test encoding a mixed-type list."""
        expected = [
            131, 108, 0, 0, 0, 3, 97, 69, 97, 42, 107, 0, 6, 97, 114,
            110, 111, 108, 100, 106]
        input_value = [69, 42, 'arnold']

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_new_reference(self):
        """Test encoding a new reference."""
        expected = [
            131, 114, 0, 3, 115, 13, 110, 111, 110, 111, 100, 101, 64,
            110, 111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 94]
        input_value = erl_codec.NewReference(
            erl_codec.Atom('nonode@nohost'), 0, [0, 0, 94])

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_function(self):
        """Test encoding an Erlang function."""
        expected = [
            131, 117, 0, 0, 0, 0, 103, 115, 13, 110, 111, 110, 111, 100,
            101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0,
            0, 0, 115, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109, 97,
            1, 97, 1]
        input_value = erl_codec.Function(
            erl_codec.Pid(
                erl_codec.Atom('nonode@nohost'), 31, 0, 0),
                erl_codec.Atom('ilia_atom'), 1, 1, None)

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_new_function(self):
        """Test encoding an Erlang lambda (created at run-time)."""
        expected = [
            131, 112, 0, 0, 0, 71, 0, 184, 143, 129, 148, 32, 145, 63,
            179, 91, 201, 150, 94, 151, 97, 58, 227, 0, 0, 0, 1, 0, 0,
            0, 0, 115, 8, 101, 114, 108, 95, 101, 118, 97, 108, 97, 20,
            98, 4, 2, 194, 168, 103, 115, 13, 110, 111, 110, 111, 100,
            101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0,
            0, 0]
        input_value = erl_codec.NewFunction(
            0, '\xb8\x8f\x81\x94 \x91?\xb3[\xc9\x96^\x97a:\xe3', 1,
            erl_codec.Atom('erl_eval'), 20, 67289768,
            erl_codec.Pid(erl_codec.Atom('nonode@nohost'), 31, 0, 0),
            [])

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_export(self):
        """Test encoding an Erlang export definition."""
        expected = [
            131, 113, 115, 6, 101, 114, 108, 97, 110, 103, 115, 4, 115,
            101, 108, 102, 97, 0]
        input_value = erl_codec.Export(
            erl_codec.Atom('erlang'), erl_codec.Atom('self'), 0)

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)

    def test_encode_dict(self):
        """Test encoding a dictionary (should create a proplist)."""
        expected = [
            131, 108, 0, 0, 0, 3, 104, 2, 107, 0, 5, 116, 104, 114, 101,
            101, 97, 3, 104, 2, 107, 0, 3, 116, 119, 111, 97, 2, 104, 2,
            107, 0, 3, 111, 110, 101, 97, 1, 106]
        input_value = {'one': 1, 'two': 2, 'three': 3}

        stream = StringIO()
        erl_codec.encode(input_value, stream)
        actual = byte_list_from_stringio(stream)
        stream.close()

        self.assertEqual(expected, actual)
