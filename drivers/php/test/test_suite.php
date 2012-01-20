<?php

require 'encode_test.php';
require 'decode_test.php';

/**
 * Runs all tests.
 */
class AllTests
{
    /**
     * Called by PHPUnit to run all tests.
     *
     * @return PHPUnit_Framework_TestSuite
     */
    public static function suite()
    {
        $suite = new PHPUnit_Framework_TestSuite();
        $suite->addTest(new PHPUnit_Framework_TestSuite('DecodeTest'));
        $suite->addTest(new PHPUnit_Framework_TestSuite('EncodeTest'));
 
        return $suite;
    }
}
