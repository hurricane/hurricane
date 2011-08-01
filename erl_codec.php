<?php

namespace Erlang;

class Exception extends \Exception {}

class StreamEmulator {
    public $data;
    public $pos;

    public function __construct($data=null) {
        $this->pos = 0;

        if (!$data) {
            $this->data = '';
        } else if (is_array($data)) {
            $this->data = to_binary($data);
        } else {
            $this->data = $data;
        }
    }

    public function read($bytes) {
        if (strlen($this->data) < $this->pos + $bytes) {
            throw new Exception(
                'Out of data to read (was asked for ' .
                $bytes . 'bytes(s), only ' .
                strlen($this->data) - $bytes . ' byte(s) remain.'
            );
        }

        $read_data = substr($this->data, $this->pos, $bytes);
        $this->pos += $bytes;
        return $read_data;
    }

    public function write($data) {
        $this->data .= $data;
    }

    public function flush() {
    }

    public function clear() {
        $this->data = '';
        $this->pos = 0;
    }
}

class AtomCacheRef {
    public $value;

    public function __construct($value) {
        $this->value = $value;
    }
}

class Atom {
    public $name;

    public function __construct($name) {
        $this->name = $name;
    }
}

class Reference {
    public $atom;
    public $identifier;
    public $creation;

    public function __construct($atom, $identifier, $creation) {
        $this->atom = $atom;
        $this->identifier = $identifier;
        $this->creation = $creation;
    }
}

class Port {
    public $atom;
    public $identifier;
    public $creation;

    public function __construct($atom, $identifier, $creation) {
        $this->atom = $atom;
        $this->identifier = $identifier;
        $this->creation = $creation;
    }
}

class Pid {
    public $atom;
    public $identifier;
    public $serial;
    public $creation;

    public function __construct($atom, $identifier, $serial, $creation) {
        $this->atom = $atom;
        $this->identifier = $identifier;
        $this->serial = $serial;
        $this->creation = $creation;
    }
}

class Tuple {
    public $data;

    public function __construct($data) {
        $this->data = $data;
    }
}

class Binary {
    public $data;

    public function __construct($data) {
        $this->data = $data;
    }
}

class NewReference {
    public $atom;
    public $creation;
    public $ids;

    public function __construct($atom, $creation, $ids) {
        $this->atom = $atom;
        $this->creation = $creation;
        $this->ids = $ids;
    }
}

class NewFunction {
    public $arity;
    public $uniq;
    public $index;
    public $module;
    public $old_index;
    public $old_uniq;
    public $pid;
    public $free_vars;

    public function __construct(
        $arity, $uniq, $index, $module, $old_index,
        $old_uniq, $pid, $free_vars
    ) {
        $this->arity = $arity;
        $this->uniq = $uniq;
        $this->index = $index;
        $this->module = $module;
        $this->old_index = $old_index;
        $this->old_uniq = $old_uniq;
        $this->pid = $pid;
        $this->free_vars = $free_vars;
    }
}

class Export {
    public $module;
    public $function;
    public $arity;

    public function __construct($module, $function, $arity) {
        $this->module = $module;
        $this->function = $function;
        $this->arity = $arity;
    }
}

class ErlFunction {
    public $pid;
    public $module;
    public $index;
    public $uniq;
    public $free_vars;

    public function __construct($pid, $module, $index, $uniq, $free_vars) {
        $this->pid = $pid;
        $this->module = $module;
        $this->index = $index;
        $this->uniq = $uniq;
        $this->free_vars = $free_vars;
    }
}

class BitBinary {
    public $bits;
    public $data;

    public function __construct($bits, $data) {
        $this->bits = $bits;
        $this->data = $data;
    }
}

function decode_atom_cache_ref($stream) {
    return new AtomCacheRef(ord($stream->read(1)));
}

function decode_small_integer_ext($stream) {
    return ord($stream->read(1));
}

function decode_integer_ext($stream) {
    $val = $stream->read(4);
    if (MACHINE_ENDIANNESS == 'LITTLE_ENDIAN') {
        $val = strrev($val);
    }
    return reset(unpack('l', $val));
}

function decode_float_ext($stream) {
    return (double) $stream->read(31);
}

function decode_atom_ext($stream) {
    $atom_len = reset(unpack('n', $stream->read(2)));
    return new Atom($stream->read($atom_len));
}

function decode_reference_ext($stream) {
    $atom = decode($stream, false);
    $identifier = reset(unpack('N', $stream->read(4)));
    $creation = ord($stream->read(1));
    return new Reference($atom, $identifier, $creation);
}

function decode_port_ext($stream) {
    $atom = decode($stream, false);
    $identifier = reset(unpack('N', $stream->read(4)));
    $creation = ord($stream->read(1));
    return new Port($atom, $identifier, $creation);
}

function decode_pid_ext($stream) {
    $atom = decode($stream, false);
    $identifier = reset(unpack('N', $stream->read(4)));
    $serial = reset(unpack('N', $stream->read(4)));
    $creation = ord($stream->read(1));
    return new Pid($atom, $identifier, $serial, $creation);
}

function decode_small_tuple_ext($stream) {
    $tuple_len = ord($stream->read(1));
    $elements = array();
    for ($i = 0; $i < $tuple_len; $i++) {
        $value = decode($stream, false);
        $elements[] = $value;
    }
    return new Tuple($elements);
}

function decode_large_tuple_ext($stream) {
    $tuple_len = reset(unpack('N', $stream->read(4)));
    $elements = array();
    for ($i = 0; $i < $tuple_len; $i++) {
        $value = decode($stream, false);
        $elements[] = $value;
    }
    return new Tuple($elements);
}

function decode_nil_ext($stream) {
    return null;
}

function decode_string_ext($stream) {
    $str_len = reset(unpack('n', $stream->read(2)));
    return $stream->read($str_len);
}

function decode_list_ext($stream) {
    $list_len = reset(unpack('N', $stream->read(4)));
    $elements = array();
    $is_str = true;
    for ($i = 0; $i < $list_len; $i++) {
        $value = decode($stream, false);
        $is_str = $is_str && is_numeric($value) && $value < 256;
        $elements[] = $value;
    }
    $tail = decode($stream, false);
    if ($tail !== null) {
        $is_str = $is_str && is_numeric($value) && $value < 256;
        $elements[] = $tail;
    }

    if ($is_str) {
        $outstr = '';
        foreach ($elements as $element) {
            $outstr .= chr($element);
        }
        return $outstr;
    } else {
        return $elements;
    }
}

function decode_binary_ext($stream) {
    $bin_len = reset(unpack('N', $stream->read(4)));
    return new Binary($stream->read($bin_len));
}

/**
 * PHP does not have unboxed longs, so we're forced to lose precision.
 */
function decode_small_big_ext($stream) {
    $num_bytes = ord($stream->read(1));
    $sign = ord($stream->read(1));
    $num = 0.0;
    for ($i = 0; $i < $num_bytes; $i++) {
        $num += (double) ord($stream->read(1)) * (double) pow(256, $i);
    }
    if ($sign == 1) {
        $num *= -1.0;
    }
    return $num;
}

/**
 * PHP does not have unboxed longs, so we're forced to lose precision.
 */
function decode_large_big_ext($stream) {
    $num_bytes = reset(unpack('N', $stream->read(4)));
    $sign = ord($stream->read(1));
    $num = 0.0;
    for ($i = 0; $i < $num_bytes; $i++) {
        $num += (double) ord($stream->read(1)) * (double) pow(256, $i);
    }
    if ($sign == 1) {
        $num *= -1.0;
    }
    return $num;
}

function decode_new_reference_ext($stream) {
    $length = reset(unpack('n', $stream->read(2)));
    $atom = decode($stream, false);
    $creation = ord($stream->read(1));
    $ids = array();
    for ($i = 0; $i < $length; $i++) {
        $id = reset(unpack('N', $stream->read(4)));
        array_unshift($ids, $id);
    }
    return new NewReference($atom, $creation, $ids);
}

function decode_small_atom_ext($stream) {
    $atom_len = ord($stream->read(1));
    $atom_name = $stream->read($atom_len);
    return new Atom($atom_name);
}

function decode_new_fun_ext($stream) {
    $size = reset(unpack('N', $stream->read(4)));
    $arity = ord($stream->read(1));
    $uniq = $stream->read(16);
    $index = reset(unpack('N', $stream->read(4)));
    $num_free = reset(unpack('N', $stream->read(4)));
    $module = decode($stream, false);
    $old_index = decode($stream, false);
    $old_uniq = decode($stream, false);
    $pid = decode($stream, false);
    $free_vars = array();
    for ($i = 0; $i < $num_free; $i++) {
        $free_var = decode($stream, false);
        $free_vars[] = $free_var;
    }
    if (!count($free_vars)) {
        $free_vars = null;
    }

    return new NewFunction(
        $arity, $uniq, $index, $module,
        $old_index, $old_uniq, $pid, $free_vars);
}

function decode_export_ext($stream) {
    $module = decode($stream, false);
    $function = decode($stream, false);
    $arity = decode($stream, false);
    return new Export($module, $function, $arity);
}

function decode_fun_ext($stream) {
    $num_free = reset(unpack('N', $stream->read(4)));
    $pid = decode($stream, false);
    $module = decode($stream, false);
    $index = decode($stream, false);
    $uniq = decode($stream, false);
    $free_vars = array();
    for ($i = 0; $i < $num_free; $i++) {
        $free_var = decode($stream, false);
        $free_vars[] = $free_var;
    }
    if (!count($free_vars)) {
        $free_vars = null;
    }

    return new ErlFunction($pid, $module, $index, $uniq, $free_vars);
}

function decode_bit_binary_ext($stream) {
    $length = reset(unpack('N', $stream->read(4)));
    return new BitBinary(ord($stream->read(1)), $stream->read($length));
}

function decode_new_float_ext($stream) {
    $data = $stream->read(8);
    if (MACHINE_ENDIANNESS == 'LITTLE_ENDIAN') {
        $data = strrev($data);
    }
    return reset(unpack('d', $data));
}

function decode($stream, $check_dist_tag=true) {
    $first_byte = ord($stream->read(1));
    if ($check_dist_tag) {
        if ($first_byte != 131) {
            throw new Exception('this is not an Erlang EXT datatype');
        } else {
            $ext_code = ord($stream->read(1));
        }
    } else {
        $ext_code = $first_byte;
    }

    if     ($ext_code == 70)  { return decode_new_float_ext($stream); }
    elseif ($ext_code == 77)  { return decode_bit_binary_ext($stream); }
    elseif ($ext_code == 82)  { return decode_atom_cache_ref($stream); }
    elseif ($ext_code == 97)  { return decode_small_integer_ext($stream); }
    elseif ($ext_code == 98)  { return decode_integer_ext($stream); }
    elseif ($ext_code == 99)  { return decode_float_ext($stream); }
    elseif ($ext_code == 100) { return decode_atom_ext($stream); }
    elseif ($ext_code == 101) { return decode_reference_ext($stream); }
    elseif ($ext_code == 102) { return decode_port_ext($stream); }
    elseif ($ext_code == 103) { return decode_pid_ext($stream); }
    elseif ($ext_code == 104) { return decode_small_tuple_ext($stream); }
    elseif ($ext_code == 105) { return decode_large_tuple_ext($stream); }
    elseif ($ext_code == 106) { return decode_nil_ext($stream); }
    elseif ($ext_code == 107) { return decode_string_ext($stream); }
    elseif ($ext_code == 108) { return decode_list_ext($stream); }
    elseif ($ext_code == 109) { return decode_binary_ext($stream); }
    elseif ($ext_code == 110) { return decode_small_big_ext($stream); }
    elseif ($ext_code == 111) { return decode_large_big_ext($stream); }
    elseif ($ext_code == 112) { return decode_new_fun_ext($stream); }
    elseif ($ext_code == 113) { return decode_export_ext($stream); }
    elseif ($ext_code == 114) { return decode_new_reference_ext($stream); }
    elseif ($ext_code == 115) { return decode_small_atom_ext($stream); }
    elseif ($ext_code == 117) { return decode_fun_ext($stream); }
    else {
        throw new Exception(
            'Unable to decode Erlang EXT data type: ' . $ext_code
        );
    }
}

function encode_float($data, $stream) {
    $stream->write(chr(70));
    $val = pack('d', $data);
    if (MACHINE_ENDIANNESS == 'LITTLE_ENDIAN') {
        $val = strrev($val);
    }
    $stream->write($val);
}

function encode_bit_binary($data, $stream) {
    $stream->write(chr(77));
    $stream->write(pack('N', strlen($data->data)));
    $stream->write(chr($data->bits));
    $stream->write($data->data);
}

function encode_atom_cache_ref($data, $stream) {
    $stream->write(chr(82));
    $stream->write(chr($data->value));
}

function encode_small_integer($data, $stream) {
    $stream->write(chr(97));
    $stream->write(chr($data));
}

function encode_integer($data, $stream) {
    $stream->write(chr(98));
    $stream->write(pack('N', $data));
}

function encode_number($data, $stream) {
    if ($data >= 0 && $data <= 0xff) {
        encode_small_integer($data, $stream);
    } elseif ($data >= -0x7fffffff - 1 && $data <= 0x7fffffff) {
        encode_integer($data, $stream);
    } else {
        encode_long($data, $stream);
    }
}

function encode_atom($data, $stream) {
    $name_len = strlen($data->name);
    if ($name_len <= 0xf) {
        $stream->write(chr(115));
        $stream->write(chr($name_len));
    } else {
        $stream->write(chr(100));
        $stream->write(pack('n', $name_len));
    }
    $stream->write($data->name);
}

function encode_reference($data, $stream) {
    $stream->write(chr(101));
    encode($data->atom, $stream, false);
    $stream->write(pack('N', $data->identifier));
    $stream->write(chr($data->creation));
}

function encode_port($data, $stream) {
    $stream->write(chr(102));
    encode($data->atom, $stream, false);
    $stream->write(pack('N', $data->identifier));
    $stream->write(chr($data->creation));
}

function encode_pid($data, $stream) {
    $stream->write(chr(103));
    encode($data->atom, $stream, false);
    $stream->write(pack('N', $data->identifier));
    $stream->write(pack('N', $data->serial));
    $stream->write(chr($data->creation));
}

function encode_tuple($data, $stream) {
    $data_len = count($data->data);
    if (count($data->data) < 256) {
        $stream->write(chr(104));
        $stream->write(chr($data_len));
    } else {
        $stream->write(chr(105));
        $stream->write(pack('N', $data_len));
    }
    foreach ($data->data as $datum) {
        encode($datum, $stream, false);
    }
}

function encode_null($data, $stream) {
    $stream->write(chr(106));
}

function encode_binary($data, $stream) {
    $stream->write(chr(109));
    $stream->write(pack('N', strlen($data->data)));
    $stream->write($data->data);
}

function encode_str($data, $stream) {
    $data_len = strlen($data);
    if ($data_len > 0xffff) {
        $stream->write(chr(108));
        $stream->write(pack('N', $data_len));
        for ($i = 0; $i < $data_len; $i++) {
            encode(ord($data[$i]), $stream, false);
        }
    } else {
        $stream->write(chr(107));
        $stream->write(pack('n', $data_len));
        $stream->write($data);
    }
}

function encode_list($data, $stream) {
    $stream->write(chr(108));
    $stream->write(pack('N', count($data)));
    foreach ($data as $value) {
        encode($value, $stream, false);
    }
    $stream->write(chr(106));
}

function encode_dict($data, $stream) {
    $stream->write(chr(108));
    $stream->write(pack('N', count($data)));
    foreach ($data as $key => $value) {
        encode(new Tuple(array($key, $value)), $stream, false);
    }
    $stream->write(chr(106));
}

function encode_array($data, $stream) {
    if (array_keys($data) == range(0, count($data) - 1)) {
        encode_list($data, $stream);
    } else {
        encode_dict($data, $stream);
    }
}

function encode_new_reference($data, $stream) {
    $stream->write(chr(114));
    $ids_len = count($data->ids);
    $stream->write(pack('n', $ids_len));
    encode($data->atom, $stream, false);
    $stream->write(chr($data->creation));
    foreach ($data->ids as $id) {
        $stream->write(pack('N', $id));
    }
}

function encode_function($data, $stream) {
    $stream->write(chr(117));
    if ($data->free_vars == null) {
        $free_vars_len = 0;
    } else {
        $free_vars_len = count($data->free_vars);
    }
    $stream->write(pack('N', $free_vars_len));
    encode($data->pid, $stream, false);
    encode($data->module, $stream, false);
    encode($data->index, $stream, false);
    encode($data->uniq, $stream, false);
    if ($free_vars_len > 0) {
        foreach ($data->free_vars as $free_var) {
            $stream->write(pack('N', $free_var));
        }
    }
}

function encode_new_function($data, $stream) {
    $stream->write(chr(112));
    if ($data->free_vars == null) {
        $free_vars_len = 0;
    } else {
        $free_vars_len = count($data->free_vars);
    }

    $bytes = new StreamEmulator();
    $bytes->write(chr($data->arity));
    $bytes->write($data->uniq);
    $bytes->write(pack('N', $data->index));
    $bytes->write(pack('N', $free_vars_len));
    encode($data->module, $bytes, false);
    encode($data->old_index, $bytes, false);
    encode($data->old_uniq, $bytes, false);
    encode($data->pid, $bytes, false);
    if ($free_vars_len > 0) {
        foreach ($data->free_vars as $free_var) {
            $bytes->write(pack('N', $free_var));
        }
    }
    $stream->write(pack('N', strlen($bytes->data) + 4));
    $stream->write($bytes->data);
}

function encode_export($data, $stream) {
    $stream->write(chr(113));
    encode($data->module, $stream, false);
    encode($data->function, $stream, false);
    encode($data->arity, $stream, false);
}

function encode($data, $stream, $send_magic_byte=true) {
    if ($send_magic_byte) {
        $stream->write(chr(131));
    }

    if     (is_float($data))               { encode_float($data, $stream); }
    elseif ($data instanceof AtomCacheRef) { encode_atom_cache_ref($data, $stream); }
    elseif (is_numeric($data))             { encode_number($data, $stream); }
    elseif ($data instanceof Atom)         { encode_atom($data, $stream); }
    elseif ($data instanceof Reference)    { encode_reference($data, $stream); }
    elseif ($data instanceof Port)         { encode_port($data, $stream); }
    elseif ($data instanceof Pid)          { encode_pid($data, $stream); }
    elseif ($data instanceof Tuple)        { encode_tuple($data, $stream); }
    elseif ($data == null)                 { encode_null($data, $stream); }
    elseif (is_string($data))              { encode_str($data, $stream); }
    elseif (is_array($data))               { encode_array($data, $stream); }
    elseif ($data instanceof Binary)       { encode_binary($data, $stream); }
    elseif ($data instanceof NewReference) { encode_new_reference($data, $stream); }
    elseif ($data instanceof ErlFunction)  { encode_function($data, $stream); }
    elseif ($data instanceof NewFunction)  { encode_new_function($data, $stream); }
    elseif ($data instanceof BitBinary)    { encode_bit_binary($data, $stream); }
    elseif ($data instanceof Export)       { encode_export($data, $stream); }
    else {
        throw new Exception($data . 'is not Erlang serializable');
    }
}

function to_binary($input) {
    $output = '';
    foreach ($input as $val) {
        $output .= chr($val);
    }
    return $output;
}

function from_binary($input) {
    $output = array();
    for ($i = 0; $i < strlen($input); $i++) {
        $output[] = ord($input[$i]);
    }
    return $output;
}

class Gateway {
    private static $instance;
    private static $in_handle;
    private static $out_handle;
    private static $stream_wrapper;

    private function __construct() {
        self::setInput('php://stdin');
        self::setOutput('php://stdout');
        self::$stream_wrapper = new StreamEmulator();
    }

    public function getInstance() {
        if (!self::$instance) {
            self::$instance = new Gateway();
        }
        return self::$instance;
    }

    public static function setInput($stream) {
        self::closeInput();

        if (is_string($stream)) {
            $stream = fopen($stream, 'r');
        }
        self::$in_handle = $stream;
    }

    public static function setOutput($stream) {
        self::closeOutput();

        if (is_string($stream)) {
            $stream = fopen($stream, 'w');
        }
        self::$out_handle = $stream;
    }

    private static function closeInput() {
        if (self::$in_handle) {
            fclose(self::$in_handle);
        }
    }

    private static function closeOutput() {
        if (self::$out_handle) {
            fclose(self::$out_handle);
        }
    }

    public function recv() {
        $message_len = fread(self::$in_handle, 4);

        if (strlen($message_len) < 4) {
            throw new Exception('Message size payload should be 4 bytes');
        }

        $message_len = reset(unpack('N', $message_len));
        self::$stream_wrapper->clear();
        self::$stream_wrapper->write(fread(self::$in_handle, $message_len));
        $message = decode(self::$stream_wrapper);
        return $message;
    }

    public function send($message) {
        self::$stream_wrapper->clear();
        encode($message, self::$stream_wrapper);
        fwrite(self::$out_handle, pack('N', strlen(self::$stream_wrapper->data)));
        fwrite(self::$out_handle, self::$stream_wrapper->data);
        fflush(self::$out_handle);
    }
}

if (!defined('MACHINE_ENDIANNESS')) {
    if (reset(unpack('L', "\x00\x00\x00\x01")) == 1) {
        define('MACHINE_ENDIANNESS', 'BIG_ENDIAN');
    } else {
        define('MACHINE_ENDIANNESS', 'LITTLE_ENDIAN');
    }
}
