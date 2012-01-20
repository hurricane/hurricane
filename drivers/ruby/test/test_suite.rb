require 'test/unit'
require 'test/unit/ui/console/testrunner'

$:.push(File.dirname(File.expand_path(__FILE__)))

require 'decode_test'
require 'encode_test'

Test::Unit::UI::Console::TestRunner.run(EncodeTest)
Test::Unit::UI::Console::TestRunner.run(DecodeTest)
