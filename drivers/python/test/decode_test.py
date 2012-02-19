"""Unit tests for the Erlang encode/decode logic."""

from hurricane import erl_codec
from unittest import TestCase
from cStringIO import StringIO


def stringio_from_bytes(byte_list):
    """Turn a list of bytes into a StringIO object."""
    return StringIO(''.join(chr(x) for x in byte_list))


class DecodeTest(TestCase):
    """Contains all test cases for decoding."""

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
