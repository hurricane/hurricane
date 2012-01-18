require 'test/unit'
require 'test/unit/ui/console/testrunner'
require 'decode_test'
require 'encode_test'

Test::Unit::UI::Console::TestRunner.run(EncodeTest)
Test::Unit::UI::Console::TestRunner.run(DecodeTest)
