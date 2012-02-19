require 'test/unit'
require 'erl_codec'

# Runs all encode tests.
class EncodeTest < Test::Unit::TestCase

  # Test that encoding a float to IEEE format works
  def test_encode_float()
    input = 1.1
    expected = [131, 70, 63, 241, 153, 153, 153, 153, 153, 154]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a bit binary (space-compressed binary) works.
  def test_encode_bit_binary()
    input = Erlang::BitBinary.new(4, "\303\213\036@")
    expected = [131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding an atom cache reference works.
  def test_encode_atom_cache_ref()
    input = Erlang::AtomCacheRef.new(1)
    expected = [131, 82, 1]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a byte works.
  def test_encode_byte()
    input = 69
    expected = [131, 97, 69]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a negative integer works.
  def test_encode_negative_integer()
    input = -69
    expected = [131, 98, 255, 255, 255, 187]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that a large integer works.
  def test_encode_large_integer()
    input = \
      '75960403121632972742224425782080432361122790418394413080455142' \
      '03595638030283176823539793587591372230230103933110810192201741' \
      '429'
    input = input.to_i()
    expected = [
      131, 110, 53, 0, 117, 116, 129, 23, 136, 197, 28, 142, 23, 72,
      229, 165, 191, 192, 22, 188, 155, 206, 43, 113, 63, 242, 3, 25,
      228, 209, 17, 66, 83, 52, 125, 56, 245, 58, 162, 52, 201, 215, 74,
      188, 3, 203, 142, 87, 67, 19, 43, 193, 5, 110, 196, 226, 44]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a long integer works.
  def test_encode_long_integer()
    input = 7446744073709551614
    expected = [131, 110, 8, 0, 254, 255, 179, 206, 71, 38, 88, 103]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that a huge integer works.
  def test_encode_huge_integer()
    input = \
      '-6901876011047750013999709330765192556067125454126993873449155' \
      '17460601040692684863445327614963592393331577611883430686726003' \
      '20404780782575195701531507559774563565628319791787601508256122' \
      '69807381115885784417937797871900125016834897398037497595394623' \
      '05385174149058805302264658966833568881166285613657489672510482' \
      '13892169887731884852288121693602037581552313916041646049767905' \
      '49656511411145133156651764786387016258072024352382315447741247' \
      '37432015958759916731431850199403869080740868507119501455074678' \
      '76786682059951287361122483612809358164954970988104000757377154' \
      '06039799899334552736365361127782308665479876394478383048878041' \
      '12238243399363922795838208755342726729867304720446459486300593' \
      '55922787741718274676482463028935591340975519484718625522498987' \
      '81957082608425105021210168804815528461538008828218661021989451' \
      '35736550581922502814829154034288730975401074903717351093798684' \
      '60699458255231373891676939153248438658256018274862785343174031' \
      '57878214138452194498676743068901891507065363033839368171702714' \
      '66729867398917277136719507329587955077462767635104476913332287' \
      '83453165006486430920406561708285077495905396599811247631747537' \
      '57783121983183284834454897800204691554455623313940955031503925' \
      '93772800374289133500332251952055634409109257341599567347188778' \
      '47137004940532369340483084127561146325281'
    input = input.to_i()
    expected = [
      131, 111, 0, 0, 2, 20, 1, 33, 89, 30, 229, 196, 107, 139, 7, 211,
      176, 94, 210, 240, 73, 193, 252, 228, 16, 46, 39, 41, 172, 134,
      15, 148, 175, 202, 174, 39, 93, 95, 64, 112, 61, 184, 30, 213,
      51, 0, 148, 67, 252, 76, 57, 30, 101, 112, 72, 159, 222, 69, 12,
      72, 158, 21, 174, 51, 215, 253, 188, 108, 235, 36, 137, 63, 135,
      237, 38, 12, 135, 171, 231, 225, 245, 246, 107, 70, 145, 37, 35,
      181, 217, 230, 237, 151, 244, 238, 71, 140, 158, 2, 236, 2, 93,
      139, 192, 182, 2, 50, 113, 43, 5, 236, 64, 243, 200, 20, 89, 177,
      118, 93, 105, 30, 252, 155, 221, 247, 170, 178, 142, 230, 55, 167,
      20, 34, 188, 215, 141, 245, 9, 222, 240, 100, 209, 43, 38, 114,
      13, 194, 226, 173, 132, 73, 15, 237, 138, 30, 190, 247, 101, 97,
      157, 20, 252, 135, 90, 24, 201, 203, 85, 23, 246, 227, 235, 87,
      138, 230, 66, 48, 114, 50, 107, 197, 216, 154, 225, 22, 25, 242,
      197, 212, 131, 185, 132, 162, 210, 65, 232, 14, 213, 216, 36, 126,
      216, 164, 16, 1, 74, 252, 199, 36, 155, 160, 252, 165, 218, 85,
      222, 27, 180, 119, 88, 156, 92, 20, 184, 163, 209, 123, 104, 43,
      183, 108, 101, 228, 132, 16, 35, 210, 22, 245, 68, 110, 45, 82,
      22, 142, 52, 122, 7, 105, 14, 206, 113, 124, 70, 167, 77, 225, 59,
      253, 180, 181, 216, 137, 245, 9, 5, 80, 206, 16, 12, 185, 153,
      155, 248, 201, 114, 96, 179, 184, 11, 186, 233, 212, 121, 204,
      235, 191, 214, 163, 41, 83, 252, 84, 84, 56, 55, 60, 119, 101, 99,
      167, 129, 41, 23, 111, 107, 225, 103, 79, 81, 142, 19, 236, 207,
      115, 250, 103, 206, 126, 5, 43, 201, 60, 114, 237, 13, 111, 210,
      211, 204, 247, 189, 195, 138, 2, 41, 139, 167, 180, 56, 59, 180,
      225, 93, 12, 69, 20, 144, 251, 15, 59, 155, 248, 59, 191, 181, 1,
      221, 181, 69, 62, 218, 157, 173, 43, 160, 213, 22, 155, 15, 1,
      173, 97, 115, 172, 243, 209, 234, 152, 42, 29, 161, 150, 134, 41,
      197, 232, 51, 208, 97, 230, 236, 83, 240, 74, 171, 221, 6, 125,
      70, 51, 42, 185, 243, 237, 232, 105, 145, 114, 181, 138, 130, 0,
      139, 216, 10, 137, 96, 156, 133, 150, 212, 79, 213, 5, 170, 161,
      40, 80, 163, 135, 65, 51, 2, 176, 182, 10, 25, 239, 40, 22, 192,
      166, 213, 250, 238, 175, 244, 201, 53, 0, 63, 196, 29, 36, 59,
      159, 202, 175, 18, 85, 170, 114, 224, 209, 94, 172, 65, 182, 244,
      181, 166, 29, 177, 235, 21, 228, 10, 247, 170, 185, 42, 151, 195,
      33, 255, 77, 5, 166, 98, 50, 4, 20, 44, 227, 203, 198, 212, 3,
      112, 48, 102, 20, 142, 238, 92, 183, 111, 39, 7, 81, 72, 166, 102,
      182, 15, 46, 10, 74, 211, 137, 20, 168, 211, 1, 105, 1, 180, 151,
      185, 129, 59, 77, 71, 42, 88, 31, 94, 147, 11]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding an atom works.
  def test_encode_atom()
    input = Erlang::Atom.new('ilia_atom')
    expected = [131, 115, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a long atom works.
  def test_encode_long_atom()
    input = Erlang::Atom.new('ilia_atom_is_quite_a_long_atom')
    expected = [
      131, 100, 0, 30, 105, 108, 105, 97, 95, 97, 116, 111, 109, 95,
      105, 115, 95, 113, 117, 105, 116, 101, 95, 97, 95, 108, 111, 110,
      103, 95, 97, 116, 111, 109]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a reference works.
  def test_encode_reference()
    input = Erlang::Reference.new(
      Erlang::Atom.new('nonode@nohost'), 69, 0)
    expected = [
      131, 101, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 0, 69, 0]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a port identifier works.
  def test_encode_port()
    input = Erlang::Port.new(Erlang::Atom.new('nonode@nohost'), 69, 0)
    expected = [
      131, 102, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 0, 69, 0]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a process identifier works.
  def test_encode_pid()
    input = Erlang::Pid.new(Erlang::Atom.new('nonode@nohost'), 69, 0, 0)
    expected = [
      131, 103, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 0, 69, 0, 0, 0, 0, 0]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a small tuple works.
  def test_encode_small_tuple()
    input = Erlang::Tuple.new([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    expected = [
      131, 104, 10, 97, 0, 97, 1, 97, 2, 97, 3, 97, 4, 97, 5, 97, 6, 97,
      7, 97, 8, 97, 9]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a large tuple works.
  def test_encode_large_tuple()
    input = Erlang::Tuple.new([
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
      19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
      51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66,
      67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
      83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
      99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
      112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
      125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137,
      138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150,
      151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163,
      164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176,
      177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
      190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202,
      203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215,
      216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228,
      229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241,
      242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
      255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267,
      268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280,
      281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293,
      294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306,
      307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319,
      320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332,
      333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345,
      346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358,
      359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371,
      372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384,
      385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397,
      398, 399])
    expected = [
      131, 105, 0, 0, 1, 144, 97, 0, 97, 1, 97, 2, 97, 3, 97, 4, 97, 5,
      97, 6, 97, 7, 97, 8, 97, 9, 97, 10, 97, 11, 97, 12, 97, 13, 97,
      14, 97, 15, 97, 16, 97, 17, 97, 18, 97, 19, 97, 20, 97, 21, 97,
      22, 97, 23, 97, 24, 97, 25, 97, 26, 97, 27, 97, 28, 97, 29, 97,
      30, 97, 31, 97, 32, 97, 33, 97, 34, 97, 35, 97, 36, 97, 37, 97,
      38, 97, 39, 97, 40, 97, 41, 97, 42, 97, 43, 97, 44, 97, 45, 97,
      46, 97, 47, 97, 48, 97, 49, 97, 50, 97, 51, 97, 52, 97, 53, 97,
      54, 97, 55, 97, 56, 97, 57, 97, 58, 97, 59, 97, 60, 97, 61, 97,
      62, 97, 63, 97, 64, 97, 65, 97, 66, 97, 67, 97, 68, 97, 69, 97,
      70, 97, 71, 97, 72, 97, 73, 97, 74, 97, 75, 97, 76, 97, 77, 97,
      78, 97, 79, 97, 80, 97, 81, 97, 82, 97, 83, 97, 84, 97, 85, 97,
      86, 97, 87, 97, 88, 97, 89, 97, 90, 97, 91, 97, 92, 97, 93, 97,
      94, 97, 95, 97, 96, 97, 97, 97, 98, 97, 99, 97, 100, 97, 101, 97,
      102, 97, 103, 97, 104, 97, 105, 97, 106, 97, 107, 97, 108, 97,
      109, 97, 110, 97, 111, 97, 112, 97, 113, 97, 114, 97, 115, 97,
      116, 97, 117, 97, 118, 97, 119, 97, 120, 97, 121, 97, 122, 97,
      123, 97, 124, 97, 125, 97, 126, 97, 127, 97, 128, 97, 129, 97,
      130, 97, 131, 97, 132, 97, 133, 97, 134, 97, 135, 97, 136, 97,
      137, 97, 138, 97, 139, 97, 140, 97, 141, 97, 142, 97, 143, 97,
      144, 97, 145, 97, 146, 97, 147, 97, 148, 97, 149, 97, 150, 97,
      151, 97, 152, 97, 153, 97, 154, 97, 155, 97, 156, 97, 157, 97,
      158, 97, 159, 97, 160, 97, 161, 97, 162, 97, 163, 97, 164, 97,
      165, 97, 166, 97, 167, 97, 168, 97, 169, 97, 170, 97, 171, 97,
      172, 97, 173, 97, 174, 97, 175, 97, 176, 97, 177, 97, 178, 97,
      179, 97, 180, 97, 181, 97, 182, 97, 183, 97, 184, 97, 185, 97,
      186, 97, 187, 97, 188, 97, 189, 97, 190, 97, 191, 97, 192, 97,
      193, 97, 194, 97, 195, 97, 196, 97, 197, 97, 198, 97, 199, 97,
      200, 97, 201, 97, 202, 97, 203, 97, 204, 97, 205, 97, 206, 97,
      207, 97, 208, 97, 209, 97, 210, 97, 211, 97, 212, 97, 213, 97,
      214, 97, 215, 97, 216, 97, 217, 97, 218, 97, 219, 97, 220, 97,
      221, 97, 222, 97, 223, 97, 224, 97, 225, 97, 226, 97, 227, 97,
      228, 97, 229, 97, 230, 97, 231, 97, 232, 97, 233, 97, 234, 97,
      235, 97, 236, 97, 237, 97, 238, 97, 239, 97, 240, 97, 241, 97,
      242, 97, 243, 97, 244, 97, 245, 97, 246, 97, 247, 97, 248, 97,
      249, 97, 250, 97, 251, 97, 252, 97, 253, 97, 254, 97, 255, 98, 0,
      0, 1, 0, 98, 0, 0, 1, 1, 98, 0, 0, 1, 2, 98, 0, 0, 1, 3, 98, 0, 0,
      1, 4, 98, 0, 0, 1, 5, 98, 0, 0, 1, 6, 98, 0, 0, 1, 7, 98, 0, 0, 1,
      8, 98, 0, 0, 1, 9, 98, 0, 0, 1, 10, 98, 0, 0, 1, 11, 98, 0, 0, 1,
      12, 98, 0, 0, 1, 13, 98, 0, 0, 1, 14, 98, 0, 0, 1, 15, 98, 0, 0,
      1, 16, 98, 0, 0, 1, 17, 98, 0, 0, 1, 18, 98, 0, 0, 1, 19, 98, 0,
      0, 1, 20, 98, 0, 0, 1, 21, 98, 0, 0, 1, 22, 98, 0, 0, 1, 23, 98,
      0, 0, 1, 24, 98, 0, 0, 1, 25, 98, 0, 0, 1, 26, 98, 0, 0, 1, 27,
      98, 0, 0, 1, 28, 98, 0, 0, 1, 29, 98, 0, 0, 1, 30, 98, 0, 0, 1,
      31, 98, 0, 0, 1, 32, 98, 0, 0, 1, 33, 98, 0, 0, 1, 34, 98, 0, 0,
      1, 35, 98, 0, 0, 1, 36, 98, 0, 0, 1, 37, 98, 0, 0, 1, 38, 98, 0,
      0, 1, 39, 98, 0, 0, 1, 40, 98, 0, 0, 1, 41, 98, 0, 0, 1, 42, 98,
      0, 0, 1, 43, 98, 0, 0, 1, 44, 98, 0, 0, 1, 45, 98, 0, 0, 1, 46,
      98, 0, 0, 1, 47, 98, 0, 0, 1, 48, 98, 0, 0, 1, 49, 98, 0, 0, 1,
      50, 98, 0, 0, 1, 51, 98, 0, 0, 1, 52, 98, 0, 0, 1, 53, 98, 0, 0,
      1, 54, 98, 0, 0, 1, 55, 98, 0, 0, 1, 56, 98, 0, 0, 1, 57, 98, 0,
      0, 1, 58, 98, 0, 0, 1, 59, 98, 0, 0, 1, 60, 98, 0, 0, 1, 61, 98,
      0, 0, 1, 62, 98, 0, 0, 1, 63, 98, 0, 0, 1, 64, 98, 0, 0, 1, 65,
      98, 0, 0, 1, 66, 98, 0, 0, 1, 67, 98, 0, 0, 1, 68, 98, 0, 0, 1,
      69, 98, 0, 0, 1, 70, 98, 0, 0, 1, 71, 98, 0, 0, 1, 72, 98, 0, 0,
      1, 73, 98, 0, 0, 1, 74, 98, 0, 0, 1, 75, 98, 0, 0, 1, 76, 98, 0,
      0, 1, 77, 98, 0, 0, 1, 78, 98, 0, 0, 1, 79, 98, 0, 0, 1, 80, 98,
      0, 0, 1, 81, 98, 0, 0, 1, 82, 98, 0, 0, 1, 83, 98, 0, 0, 1, 84,
      98, 0, 0, 1, 85, 98, 0, 0, 1, 86, 98, 0, 0, 1, 87, 98, 0, 0, 1,
      88, 98, 0, 0, 1, 89, 98, 0, 0, 1, 90, 98, 0, 0, 1, 91, 98, 0, 0,
      1, 92, 98, 0, 0, 1, 93, 98, 0, 0, 1, 94, 98, 0, 0, 1, 95, 98, 0,
      0, 1, 96, 98, 0, 0, 1, 97, 98, 0, 0, 1, 98, 98, 0, 0, 1, 99, 98,
      0, 0, 1, 100, 98, 0, 0, 1, 101, 98, 0, 0, 1, 102, 98, 0, 0, 1,
      103, 98, 0, 0, 1, 104, 98, 0, 0, 1, 105, 98, 0, 0, 1, 106, 98, 0,
      0, 1, 107, 98, 0, 0, 1, 108, 98, 0, 0, 1, 109, 98, 0, 0, 1, 110,
      98, 0, 0, 1, 111, 98, 0, 0, 1, 112, 98, 0, 0, 1, 113, 98, 0, 0, 1,
      114, 98, 0, 0, 1, 115, 98, 0, 0, 1, 116, 98, 0, 0, 1, 117, 98, 0,
      0, 1, 118, 98, 0, 0, 1, 119, 98, 0, 0, 1, 120, 98, 0, 0, 1, 121,
      98, 0, 0, 1, 122, 98, 0, 0, 1, 123, 98, 0, 0, 1, 124, 98, 0, 0, 1,
      125, 98, 0, 0, 1, 126, 98, 0, 0, 1, 127, 98, 0, 0, 1, 128, 98, 0,
      0, 1, 129, 98, 0, 0, 1, 130, 98, 0, 0, 1, 131, 98, 0, 0, 1, 132,
      98, 0, 0, 1, 133, 98, 0, 0, 1, 134, 98, 0, 0, 1, 135, 98, 0, 0, 1,
      136, 98, 0, 0, 1, 137, 98, 0, 0, 1, 138, 98, 0, 0, 1, 139, 98, 0,
      0, 1, 140, 98, 0, 0, 1, 141, 98, 0, 0, 1, 142, 98, 0, 0, 1, 143]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a nil value works.
  def test_encode_nil()
    input = nil
    expected = [131, 106]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a binary value works.
  def test_encode_binary()
    input = Erlang::Binary.new('abcd')
    expected = [131, 109, 0, 0, 0, 4, 97, 98, 99, 100]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a string works.
  def test_encode_string()
    input = 'now get your ass to mars'
    expected = [
      131, 107, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121, 111,
      117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97, 114, 115]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a list with mixed data works.
  def test_encode_list()
    input = [69, 42, 'arnold']
    expected = [
      131, 108, 0, 0, 0, 3, 97, 69, 97, 42, 107, 0, 6, 97, 114, 110,
      111, 108, 100, 106]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a new reference works.
  def test_encode_new_reference()
    input = Erlang::NewReference.new(
      Erlang::Atom.new('nonode@nohost'), 0, [0, 0, 94])
    expected = [
      131, 114, 0, 3, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110,
      111, 104, 111, 115, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding an Erlang function works.
  def test_encode_function()
    input = Erlang::Function.new(
      Erlang::Pid.new(Erlang::Atom.new('nonode@nohost'), 31, 0, 0),
      Erlang::Atom.new('ilia_atom'), 1, 1, [])
    expected = [
      131, 117, 0, 0, 0, 0, 103, 115, 13, 110, 111, 110, 111, 100, 101,
      64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0, 115,
      9, 105, 108, 105, 97, 95, 97, 116, 111, 109, 97, 1, 97, 1]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding an Erlang lambda created at run-time works.
  def test_encode_new_function()
    input = Erlang::NewFunction.new(
      0, "\270\217\201\224 \221?\263[\311\226^\227a:\343", 1,
      Erlang::Atom.new('erl_eval'), 20, 67289768,
      Erlang::Pid.new(Erlang::Atom.new('nonode@nohost'), 31, 0, 0), [])
    expected = [
      131, 112, 0, 0, 0, 71, 0, 184, 143, 129, 148, 32, 145, 63, 179,
      91, 201, 150, 94, 151, 97, 58, 227, 0, 0, 0, 1, 0, 0, 0, 0, 115,
      8, 101, 114, 108, 95, 101, 118, 97, 108, 97, 20, 98, 4, 2, 194,
      168, 103, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding an export specifier works.
  def test_encode_export()
    input = Erlang::Export.new(
      Erlang::Atom.new('erlang'), Erlang::Atom.new('self'), 0)
    expected = [
      131, 113, 115, 6, 101, 114, 108, 97, 110, 103, 115, 4, 115, 101,
      108, 102, 97, 0]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

  # Test that encoding a hash works.
  def test_encode_hash()
    input = {'three' => 3, 'two' => 2, 'one' => 1}
    expected = [
      131, 108, 0, 0, 0, 3, 104, 2, 107, 0, 3, 111, 110, 101, 97, 1,
      104, 2, 107, 0, 5, 116, 104, 114, 101, 101, 97, 3, 104, 2, 107, 0,
      3, 116, 119, 111, 97, 2, 106]

    stream = Erlang::StreamEmulator.new()
    Erlang::encode(input, stream)
    actual = stream.data.bytes().entries()

    assert_equal(expected, actual)
  end

end
