require 'test/unit'
require 'erl_codec'

# Runs all decode tests.
class DecodeTest < Test::Unit::TestCase

  # Test that decoding an atom cache reference works.
  def test_decode_atom_cache_ref()
    input = [131, 82, 1]
    expected = Erlang::AtomCacheRef.new(1)

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a byte works.
  def test_decode_byte()
    input = [131, 97, 123]
    expected = 123

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding an integer works.
  def test_decode_integer()
    input = [131, 98, 0, 0, 4, 210]
    expected = 1234

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a negative integer works.
  def test_decode_negative_integer()
    input = [131, 98, 255, 255, 255, 187]
    expected = -69

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a float as a string works.
  def test_decode_float()
    input = [
      131, 99, 49, 46, 49, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
      48, 48, 48, 48, 56, 56, 56, 50, 101, 43, 48, 48, 0, 0, 0, 0, 0]
    expected = 1.1

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding an atom works.
  def test_decode_atom()
    input = [131, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109]
    expected = Erlang::Atom.new('ilia_atom')

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a reference works.
  def test_decode_reference()
    input = [
      131, 101, 100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109, 1,
      1, 1, 1, 42]
    expected = Erlang::Reference.new(
      Erlang::Atom.new('ilia_atom'), 16843009, 42)

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a port identifier works.
  def test_decode_port()
    input = [
      131, 102, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 1, 245, 0]
    expected = Erlang::Port.new(
      Erlang::Atom.new('nonode@nohost'), 501, 0)

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a process identifier works.
  def test_decode_pid()
    input = [
      131, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0]
    expected = Erlang::Pid.new(
      Erlang::Atom.new('nonode@nohost'), 31, 0, 0)

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a small tuple works.
  def test_decode_small_tuple()
    input = [131, 104, 2, 97, 42, 97, 69]
    expected = Erlang::Tuple.new([42, 69])

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a large tuple works.
  def test_decode_large_tuple()
    input = [
      131, 105, 0, 0, 1, 26, 97, 42, 97, 69, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
      97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97,
      69]
    expected = Erlang::Tuple.new([
      42, 69, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 69])

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a nil/empty list works.
  def test_decode_nil()
    input = [131, 106]
    expected = nil

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a string works.
  def test_decode_string()
    input = [
      131, 107, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121, 111,
      117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97, 114, 115]
    expected = "now get your ass to mars"

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a list works.
  def test_decode_list()
    input = [131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97, 69, 97, 42, 106]
    expected = [1024, 69, 42]

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a list with a trailer works.
  def test_decode_list_trailer()
    input = [
      131, 108, 0, 0, 0, 3, 98, 0, 0, 4, 0, 97, 69, 97, 42, 97, 1]
    expected = [1024, 69, 42, 1]

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a binary works.
  def test_decode_binary()
    input = [
      131, 109, 0, 0, 0, 24, 110, 111, 119, 32, 103, 101, 116, 32, 121,
      111, 117, 114, 32, 97, 115, 115, 32, 116, 111, 32, 109, 97, 114,
      115]
    expected = Erlang::Binary.new('now get your ass to mars')

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a big integer works.
  def test_decode_small_big_integer()
    input = [131, 110, 5, 0, 5, 228, 183, 122, 4]
    expected = 19238740997

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a huge integer works.
  def test_decode_large_big_integer()
    input = [
      131, 111, 0, 0, 1, 2, 0, 77, 166, 39, 216, 70, 78, 172, 93, 52,
      181, 124, 101, 170, 246, 159, 16, 153, 154, 36, 214, 242, 85, 167,
      60, 82, 247, 27, 245, 116, 125, 142, 21, 240, 221, 108, 192, 132,
      152, 51, 154, 161, 28, 103, 117, 214, 153, 173, 30, 239, 99, 160,
      203, 22, 156, 85, 168, 87, 248, 230, 10, 115, 56, 202, 134, 245,
      68, 118, 184, 171, 238, 51, 180, 1, 81, 139, 164, 161, 186, 231,
      229, 45, 235, 244, 167, 92, 107, 159, 27, 27, 195, 22, 23, 236,
      229, 112, 138, 40, 150, 70, 107, 175, 46, 27, 67, 83, 124, 247,
      18, 87, 144, 167, 18, 212, 14, 129, 63, 190, 119, 149, 68, 165,
      43, 152, 211, 55, 3, 54, 145, 143, 121, 37, 88, 178, 78, 178, 210,
      187, 209, 95, 180, 82, 43, 15, 124, 42, 219, 102, 36, 195, 96, 61,
      131, 32, 224, 3, 92, 102, 31, 168, 163, 134, 238, 181, 13, 36, 71,
      28, 218, 181, 142, 191, 71, 241, 91, 166, 118, 178, 91, 243, 2,
      40, 76, 251, 97, 47, 34, 78, 233, 253, 137, 228, 231, 176, 73,
      165, 147, 17, 247, 147, 24, 66, 100, 211, 64, 134, 14, 184, 130,
      153, 249, 87, 2, 246, 140, 79, 220, 174, 26, 78, 214, 64, 224, 88,
      139, 150, 122, 184, 197, 160, 31, 203, 192, 9, 112, 34, 66, 156,
      248, 131, 70, 190, 215, 162, 230, 162, 74, 181, 184, 36, 105, 229,
      7, 168, 75, 47, 80, 83, 2]
    expected = \
      '19238740999123740918273049817230948712039847012938740192837401' \
      '92837409182374098127304985609817230984610293847120937840981273' \
      '64098712304987120394861023984701293561029387502983750921837409' \
      '81273409812309562039857102394810927401923874091283675091283765' \
      '09812730548172309461203985710293865129837405912863509182763059' \
      '18263059126305971260951623098712039857120936501293650912836750' \
      '91826350928136509128365091826305981263509182736509817230901274' \
      '01293874091283740912836740912837640981273409812730498127340902' \
      '38740918237409812730498172304987213094871230948712309487123098' \
      '56012936509518632095861203958620395620398576019234870213977677'
    expected = expected.to_i()

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a new reference works.
  def test_decode_new_reference()
    input = [
      131, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110,
      111, 104, 111, 115, 116, 0, 0, 0, 0, 94, 0, 0, 0, 0, 0, 0, 0, 0]
    expected = Erlang::NewReference.new(
      Erlang::Atom.new('nonode@nohost'), 0, [0, 0, 94])

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a short atom works.
  def test_decode_short_atom()
    input = [131, 115, 4, 97, 98, 99, 100]
    expected = Erlang::Atom.new('abcd')

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a lambda created at run-time works.
  def test_decode_new_function()
    input = [
      131, 112, 0, 0, 0, 71, 0, 184, 143, 129, 148, 32, 145, 63, 179,
      91, 201, 150, 94, 151, 97, 58, 227, 0, 0, 0, 1, 0, 0, 0, 0, 115,
      8, 101, 114, 108, 95, 101, 118, 97, 108, 97, 20, 98, 4, 2, 194,
      168, 103, 115, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
      104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0]
    expected = Erlang::NewFunction.new(
      0, "\270\217\201\224 \221?\263[\311\226^\227a:\343", 1,
      Erlang::Atom.new('erl_eval'), 20, 67289768,
      Erlang::Pid.new(Erlang::Atom.new('nonode@nohost'), 31, 0, 0), [])

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding an export specifier works.
  def test_decode_export()
    input = [
      131, 113, 100, 0, 6, 101, 114, 108, 97, 110, 103, 100, 0, 4, 115,
      101, 108, 102, 97, 0]
    expected = Erlang::Export.new(
      Erlang::Atom.new('erlang'), Erlang::Atom.new('self'), 0)

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a function works.
  def test_decode_function()
    input = [
      131, 117, 0, 0, 0, 0, 103, 100, 0, 13, 110, 111, 110, 111, 100,
      101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0, 0, 0,
      100, 0, 9, 105, 108, 105, 97, 95, 97, 116, 111, 109, 97, 1, 97, 1]
    expected = Erlang::Function.new(
      Erlang::Pid.new(Erlang::Atom.new('nonode@nohost'), 31, 0, 0),
      Erlang::Atom.new('ilia_atom'), 1, 1, [])

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test that decoding a bit binary (space-compressed binary) works.
  def test_decode_bit_binary()
    input = [131, 77, 0, 0, 0, 4, 4, 195, 139, 30, 64]
    expected = Erlang::BitBinary.new(4, "\303\213\036@")

    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_equal(expected, actual)
  end

  # Test decoding a float in the IEEE format works.
  def test_decode_new_float()
    input = [131, 70, 1, 1, 1, 1, 1, 1, 1, 1]
    expected = 7.74860418548935e-304
    stream = Erlang::StreamEmulator.new(input)
    actual = Erlang::decode(stream)

    assert_in_delta(expected, actual, 0.00000000000001)
  end

end
