<?php

/**
 * Implementation of the Erlang binary protocol.
 *
 * Provides facilities to work with Standard I/O streams, sockets, and
 * Erlang binary messages.
 */
namespace Erlang;

if (!defined('MACHINE_ENDIANNESS')) {
    if (reset(unpack('L', "\x00\x00\x00\x01")) == 1) {
        define('MACHINE_ENDIANNESS', 'BIG_ENDIAN');
    } else {
        define('MACHINE_ENDIANNESS', 'LITTLE_ENDIAN');
    }
}

/**
 * Provides an Exception type that can be thrown and caught for
 * Erlang-related operations.
 */
class Exception extends \Exception {}

/**
 * Defines an interface that must be implemented for any object that
 * wants to serve as the transport layer for the Gateway.
 */
interface StreamInterface {
    /**
     * Read bytes number of data and return it.
     *
     * @param $bytes integer
     *
     * @return string
     */
    public function read($num);

    /**
     * Write binary data to the stream.
     *
     * @param $data string
     *
     * @return void
     */
    public function write($data);

    /**
     * Write all buffered data to the output device.
     *
     * @return void
     */
    public function flush();

    /**
     * Close the output device.
     *
     * @return void
     */
    public function close();
}

/**
 * Emulates a stream. Highly useful for debugging.
 */
class StreamEmulator implements StreamInterface {
    /**
     * The data buffer used to store bytes.
     *
     * @var string
     */
    public $data;

    /**
     * The current position in the data buffer (for reads).
     *
     * @var integer
     */
    public $pos;

    /**
     * Initialize the stream emulator with an optional data argument.
     *
     * @param $data string|array
     *
     * @return void
     */
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

    /**
     * Read bytes number of data and return it. Throw an Exception if there
     * aren't enough bytes to be read.
     *
     * @param $bytes integer
     *
     * @return string
     */
    public function read($bytes) {
        if (strlen($this->data) < $this->pos + $bytes) {
            throw new Exception(
                'Out of data to read (was asked for ' .
                $bytes . ' bytes(s), only ' .
                (strlen($this->data) - $this->pos) . ' byte(s) remain.'
            );
        }

        $read_data = substr($this->data, $this->pos, $bytes);
        $this->pos += $bytes;
        return $read_data;
    }

    /**
     * Write binary data to the stream.
     *
     * @param $data string
     *
     * @return void
     */
    public function write($data) {
        $this->data .= $data;
    }

    /**
     * Exist for interface completeness.
     *
     * @return void
     */
    public function flush() {
    }

    /**
     * Reset the position and clear the data buffer.
     *
     * @return void
     */
    public function clear() {
        $this->data = '';
        $this->pos = 0;
    }

    /**
     * Exist for interface completeness.
     *
     * @return void
     */
    public function close() {}
}

/**
 * Wraps Standard I/O input and output facilities. Serves as a Gateway
 * transport.
 */
class StdioWrapper implements StreamInterface {
    /**
     * A handle to the Standard In stream.
     *
     * @var resource
     */
    private $in;

    /**
     * A handle to the Standard Out stream.
     *
     * @var resource
     */
    private $out;

    /**
     * Open Standard In and Standard Out.
     *
     * @return void
     */
    public function __construct() {
        $this->in = fopen('php://stdin', 'r');
        $this->out = fopen('php://stdout', 'w');
    }

    /**
     * Close Standard In and Standard Out.
     *
     * @return void
     */
    public function __destruct() {
        $this->close();
    }

    /**
     * Read data from the Standard In stream.
     *
     * @param integer $num
     *
     * @return string
     */
    public function read($num) {
        return fread($this->in, $num);
    }

    /**
     * Write data to the Standard Out stream.
     *
     * @param string $data
     *
     * @return void
     */
    public function write($data) {
        return fwrite($this->out, $data);
    }

    /**
     * Flush all data in the Standard Out buffer to the Standard Out
     * stream.
     *
     * @return void
     */
    public function flush() {
        fflush($this->out);
    }

    /**
     * Close Standard In and Standard Out.
     *
     * @return void
     */
    public function close() {
        fclose($this->in);
        fclose($this->out);
    }
}

/**
 * Wraps socket creation and usage logic. Serves as a Gateway transport.
 */
class SocketWrapper implements StreamInterface {
    /**
     * The socket over which communication is occurring.
     *
     * @var resource
     */
    private $socket;

    /**
     * Open a socket to the given host and port.
     *
     * @param string $host
     * @param integer $port
     *
     * @return void
     */
    public function __construct($host, $port) {
        $this->socket = fsockopen($host, $port);
    }

    /**
     * Close the socket.
     *
     * @return void
     */
    public function __destruct() {
        $this->close();
    }

    /**
     * Read data from the socket.
     *
     * @param integer $num
     *
     * @return string
     */
    public function read($num) {
        $chunks = array();
        $len_read_so_far = 0;
        while ($len_read_so_far < $num) {
            $chunk = fread($this->socket, $num - $len_read_so_far);
            $len_read_so_far += strlen($chunk);
            $chunks[] = $chunk;
        }
        return implode('', $chunks);
    }

    /**
     * Write data to the socket.
     *
     * @param string $data
     *
     * @return string
     */
    public function write($data) {
        return fwrite($this->socket, $data);
    }

    /**
     * Exist for interface completeness.
     *
     * @return void
     */
    public function flush() {
        fflush($this->socket);
    }

    /**
     * Close the socket.
     *
     * @return void
     */
    public function close() {
        fclose($this->socket);
    }
}

/**
 * The interface to implement for object that know how to transform
 * themselves into a data structure that the Erlang encoding functions
 * know how to encode.
 */
interface Serializable {
    /**
     * The public function that returns a value to serialize as Erlang
     * terms.
     */
    public function toErlang();
}

/**
 * Implements an Erlang atom cache ref.
 */
class AtomCacheRef {
    /**
     * @var integer
     */
    public $value;

    /**
     * Set the given data on the object.
     *
     * @param integer $value
     *
     * @return void
     */
    public function __construct($value) {
        $this->value = $value;
    }
}

/**
 * Implements an Erlang atom.
 */
class Atom {
    /**
     * @var string
     */
    public $name;

    /**
     * Set the given data on the object.
     *
     * @param string $name
     *
     * @return void
     */
    public function __construct($name) {
        $this->name = $name;
    }
}

/**
 * Implements an Erlang reference.
 */
class Reference {
    /**
     * @var Atom
     */
    public $atom;

    /**
     * @var integer
     */
    public $identifier;

    /**
     * @var integer
     */
    public $creation;

    /**
     * Set the given data on the object.
     *
     * @param Atom $atom
     * @param integer $identifier
     * @param integer $creation
     *
     * @return void
     */
    public function __construct($atom, $identifier, $creation) {
        $this->atom = $atom;
        $this->identifier = $identifier;
        $this->creation = $creation;
    }
}

/**
 * Implements an Erlang port.
 */
class Port {
    /**
     * @var Atom
     */
    public $atom;

    /**
     * @var integer
     */
    public $identifier;

    /**
     * @var integer
     */
    public $creation;

    /**
     * Set the given data on the object.
     *
     * @param Atom $atom
     * @param integer $identifier
     * @param integer $creation
     *
     * @return void
     */
    public function __construct($atom, $identifier, $creation) {
        $this->atom = $atom;
        $this->identifier = $identifier;
        $this->creation = $creation;
    }
}

/**
 * Implements an Erlang pid.
 */
class Pid {
    /**
     * @var Atom
     */
    public $atom;

    /**
     * @var integer
     */
    public $identifier;

    /**
     * @var integer
     */
    public $serial;

    /**
     * @var integer
     */
    public $creation;

    /**
     * Set the given data on the object.
     *
     * @param Atom $atom
     * @param integer $identifier
     * @param integer $serial
     * @param integer $creation
     *
     * @return void
     */
    public function __construct($atom, $identifier, $serial, $creation) {
        $this->atom = $atom;
        $this->identifier = $identifier;
        $this->serial = $serial;
        $this->creation = $creation;
    }
}

/**
 * Implements a tuple object to be used with Erlang messaging.
 */
class Tuple {
    /**
     * @var array
     */
    public $data;

    /**
     * Set the given data on the object.
     *
     * @param array $data
     *
     * @return void
     */
    public function __construct($data) {
        $this->data = $data;
    }
}

/**
 * Implements an Erlang binary.
 */
class Binary {
    /**
     * @var string
     */
    public $data;

    /**
     * Set the given data on the object.
     *
     * @param string $data
     *
     * @return void
     */
    public function __construct($data) {
        $this->data = $data;
    }
}

/**
 * Implements an Erlang "new reference" (a reference created at runtime).
 */
class NewReference {
    /**
     * @var Atom
     */
    public $atom;

    /**
     * @var integer
     */
    public $creation;

    /**
     * @var array
     */
    public $ids;

    /**
     * Set the given data on the object.
     *
     * @param Atom $atom
     * @param integer $creation
     * @param array $ids
     *
     * @return void
     */
    public function __construct($atom, $creation, $ids) {
        $this->atom = $atom;
        $this->creation = $creation;
        $this->ids = $ids;
    }
}

/**
 * Implements an Erlang function (created at run-time, usually with
 * the fun () -> end syntax).
 */
class NewFunction {
    /**
     * @var integer
     */
    public $arity;

    /**
     * @var string
     */
    public $uniq;

    /**
     * @var integer
     */
    public $index;

    /**
     * @var Atom
     */
    public $module;

    /**
     * @var integer
     */
    public $old_index;

    /**
     * @var integer
     */
    public $old_uniq;

    /**
     * @var Pid
     */
    public $pid;

    /**
     * @var array
     */
    public $free_vars;

    /**
     * Set the given data on the object.
     *
     * @param integer $arity
     * @param string $uniq
     * @param integer $index
     * @param Atom $module
     * @param integer $old_index
     * @param integer $old_uniq
     * @param Pid $pid
     * @param array $free_vars
     *
     * @return void
     */
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

/**
 * Implements an Erlang export.
 */
class Export {
    /**
     * @var Atom
     */
    public $module;

    /**
     * @var Atom
     */
    public $function;

    /**
     * @var integer
     */
    public $arity;

    /**
     * Set the given data on the object.
     *
     * @param Atom $module
     * @param Atom $function
     * @param integer $arity
     *
     * @return void
     */
    public function __construct($module, $function, $arity) {
        $this->module = $module;
        $this->function = $function;
        $this->arity = $arity;
    }
}

/**
 * Implements an Erlang function (defined at compile-time).
 */
class ErlFunction {
    /**
     * @var Pid
     */
    public $pid;

    /**
     * @var Atom
     */
    public $module;

    /**
     * @var integer
     */
    public $index;

    /**
     * @var integer
     */
    public $uniq;

    /**
     * @var array
     */
    public $free_vars;

    /**
     * Set the given data on the object.
     *
     * @param Pid $pid
     * @param Atom $module
     * @param integer $index
     * @param integer $uniq
     * @param array $free_vars
     *
     * @return void
     */
    public function __construct($pid, $module, $index, $uniq, $free_vars) {
        $this->pid = $pid;
        $this->module = $module;
        $this->index = $index;
        $this->uniq = $uniq;
        $this->free_vars = $free_vars;
    }
}

/**
 * Implements an Erlang bit binary.
 */
class BitBinary {
    /**
     * @var integer
     */
    public $bits;

    /**
     * @var string
     */
    public $data;

    /**
     * Set the given data on the object.
     *
     * @param integer $bits
     * @param string $data
     *
     * @return void
     */
    public function __construct($bits, $data) {
        $this->bits = $bits;
        $this->data = $data;
    }
}

/**
 * Turn an Erlang-style property list into a map.
 *
 * @param array $input
 *
 * @return array
 */
function proplistToArray(array $input) {
    $result = array();
    foreach ($input as $element) {
        if (count($element->data) < 2) {
            throw new Exception(
                'Proplist elements should have at least 2 elements'
            );
        }
        $result[$element->data[0]] = $element->data[1];
    }
    return $result;
}

/**
 * Decode and return an Erlang atom cache ref.
 *
 * @param StreamInterface $stream
 *
 * @return AtomCacheRef
 */
function decode_atom_cache_ref(StreamInterface $stream) {
    return new AtomCacheRef(ord($stream->read(1)));
}

/**
 * Decode and return a small integer (byte).
 *
 * @param StreamInterface $stream
 *
 * @return integer
 */
function decode_small_integer_ext(StreamInterface $stream) {
    return ord($stream->read(1));
}

/**
 * Decode and return an integer.
 *
 * @param StreamInterface $stream
 *
 * @return integer
 */
function decode_integer_ext(StreamInterface $stream) {
    $val = $stream->read(4);
    if (MACHINE_ENDIANNESS == 'LITTLE_ENDIAN') {
        $val = strrev($val);
    }
    return reset(unpack('l', $val));
}

/**
 * Decode and return a float (represented by Erlang as a string).
 *
 * @param StreamInterface $stream
 *
 * @return double
 */
function decode_float_ext(StreamInterface $stream) {
    return (double) $stream->read(31);
}

/**
 * Decode and return an Erlang atom.
 *
 * @param StreamInterface $stream
 *
 * @return Atom
 */
function decode_atom_ext(StreamInterface $stream) {
    $atom_len = reset(unpack('n', $stream->read(2)));
    return new Atom($stream->read($atom_len));
}

/**
 * Decode and return an Erlang reference.
 *
 * @param StreamInterface $stream
 *
 * @return Reference
 */
function decode_reference_ext(StreamInterface $stream) {
    $atom = decode($stream, false);
    $identifier = reset(unpack('N', $stream->read(4)));
    $creation = ord($stream->read(1));
    return new Reference($atom, $identifier, $creation);
}

/**
 * Decode and return an Erlang port.
 *
 * @param StreamInterface $stream
 *
 * @return Port
 */
function decode_port_ext(StreamInterface $stream) {
    $atom = decode($stream, false);
    $identifier = reset(unpack('N', $stream->read(4)));
    $creation = ord($stream->read(1));
    return new Port($atom, $identifier, $creation);
}

/**
 * Decode and return an Erlang pid.
 *
 * @param StreamInterface $stream
 *
 * @return Pid
 */
function decode_pid_ext(StreamInterface $stream) {
    $atom = decode($stream, false);
    $identifier = reset(unpack('N', $stream->read(4)));
    $serial = reset(unpack('N', $stream->read(4)));
    $creation = ord($stream->read(1));
    return new Pid($atom, $identifier, $serial, $creation);
}

/**
 * Decode and return a small Erlang tuple (fewer than 256 elements).
 *
 * @param StreamInterface $stream
 *
 * @return Tuple
 */
function decode_small_tuple_ext(StreamInterface $stream) {
    $tuple_len = ord($stream->read(1));
    $elements = array();
    for ($i = 0; $i < $tuple_len; $i++) {
        $value = decode($stream, false);
        $elements[] = $value;
    }
    return new Tuple($elements);
}

/**
 * Decode and return a large Erlang tuple (more than 256 elements).
 *
 * @param StreamInterface $stream
 *
 * @return Tuple
 */
function decode_large_tuple_ext(StreamInterface $stream) {
    $tuple_len = reset(unpack('N', $stream->read(4)));
    $elements = array();
    for ($i = 0; $i < $tuple_len; $i++) {
        $value = decode($stream, false);
        $elements[] = $value;
    }
    return new Tuple($elements);
}

/**
 * Decode and return a nil/null/None.
 *
 * @param StreamInterface $stream
 *
 * @return null
 */
function decode_nil_ext(StreamInterface $stream) {
    return null;
}

/**
 * Decode and return a string.
 *
 * @param StreamInterface $stream
 *
 * @return string
 */
function decode_string_ext(StreamInterface $stream) {
    $str_len = reset(unpack('n', $stream->read(2)));
    return $stream->read($str_len);
}

/**
 * Decode and return a list.
 *
 * Depending on the list contents, a string may be returned. This will
 * be the case if the list contains only byte values, which means that
 * the list is actually intending to be a string, but being capped by
 * Erlang's 65K char limit for strings (before they overflow into a list).
 *
 * @param StreamInterface $stream
 *
 * @return array
 */
function decode_list_ext(StreamInterface $stream) {
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

/**
 * Decode and return an Erlang binary.
 *
 * @param StreamInterface $stream
 *
 * @return Binary
 */
function decode_binary_ext(StreamInterface $stream) {
    $bin_len = reset(unpack('N', $stream->read(4)));
    return new Binary($stream->read($bin_len));
}

/**
 * Decode and return "small" big number. Uses doubles to store results,
 * as PHP does not have a big integer type.
 *
 * @param StreamInterface $stream
 *
 * @return double
 */
function decode_small_big_ext(StreamInterface $stream) {
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
 * Decode and return "large" big number. Uses doubles to store results,
 * as PHP does not have a big integer type.
 *
 * @param StreamInterface $stream
 *
 * @return integer
 */
function decode_large_big_ext(StreamInterface $stream) {
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

/**
 * Decode and return an Erlang "new reference".
 *
 * @param StreamInterface $stream
 *
 * @return NewReference
 */
function decode_new_reference_ext(StreamInterface $stream) {
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

/**
 * Decode and return a small Erlang atom.
 *
 * @param StreamInterface $stream
 *
 * @return Atom
 */
function decode_small_atom_ext(StreamInterface $stream) {
    $atom_len = ord($stream->read(1));
    $atom_name = $stream->read($atom_len);
    return new Atom($atom_name);
}

/**
 * Decode and return an Erlang "new function".
 *
 * @param StreamInterface $stream
 *
 * @return NewFunction
 */
function decode_new_fun_ext(StreamInterface $stream) {
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

/**
 * Decode and return an Erlang export.
 *
 * @param StreamInterface $stream
 *
 * @return Export
 */
function decode_export_ext(StreamInterface $stream) {
    $module = decode($stream, false);
    $function = decode($stream, false);
    $arity = decode($stream, false);
    return new Export($module, $function, $arity);
}

/**
 * Decode and return an Erlang function.
 *
 * @param StreamInterface $stream
 *
 * @return ErlFunction
 */
function decode_fun_ext(StreamInterface $stream) {
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

/**
 * Decode and return an Erlang bit binary.
 *
 * @param StreamInterface $stream
 *
 * @return BitBinary
 */
function decode_bit_binary_ext(StreamInterface $stream) {
    $length = reset(unpack('N', $stream->read(4)));
    return new BitBinary(ord($stream->read(1)), $stream->read($length));
}

/**
 * Decode and return an IEEE 8-byte floating-point number.
 *
 * @param StreamInterface $stream
 *
 * @return float
 */
function decode_new_float_ext(StreamInterface $stream) {
    $data = $stream->read(8);
    if (MACHINE_ENDIANNESS == 'LITTLE_ENDIAN') {
        $data = strrev($data);
    }
    return reset(unpack('d', $data));
}

/**
 * Decode a single value from the given stream and return it.
 *
 * If check_dist_tag, check to see that the first byte is 131 (this is
 * how Erlang flags the beginning of every data type). This check does
 * not need to be performed when recursively decoding nested data types,
 * hence the optional argument.
 *
 * @param StreamInterface $stream
 * @param boolean $check_dist_tag
 *
 * @return mixed
 */
function decode(StreamInterface $stream, $check_dist_tag=true) {
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

    switch ($ext_code) {
        case 70: return decode_new_float_ext($stream);
        case 77: return decode_bit_binary_ext($stream);
        case 82: return decode_atom_cache_ref($stream);
        case 97: return decode_small_integer_ext($stream);
        case 98: return decode_integer_ext($stream);
        case 99: return decode_float_ext($stream);
        case 100: return decode_atom_ext($stream);
        case 101: return decode_reference_ext($stream);
        case 102: return decode_port_ext($stream);
        case 103: return decode_pid_ext($stream);
        case 104: return decode_small_tuple_ext($stream);
        case 105: return decode_large_tuple_ext($stream);
        case 106: return decode_nil_ext($stream);
        case 107: return decode_string_ext($stream);
        case 108: return decode_list_ext($stream);
        case 109: return decode_binary_ext($stream);
        case 110: return decode_small_big_ext($stream);
        case 111: return decode_large_big_ext($stream);
        case 112: return decode_new_fun_ext($stream);
        case 113: return decode_export_ext($stream);
        case 114: return decode_new_reference_ext($stream);
        case 115: return decode_small_atom_ext($stream);
        case 117: return decode_fun_ext($stream);
        default:
            throw new Exception(
                'Unable to decode Erlang EXT data type: ' . $ext_code
            );
    }
}

/**
 * Encode a floating-point number into the stream.
 *
 * @param float $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_float($data, StreamInterface $stream) {
    $stream->write(chr(70));
    $val = pack('d', $data);
    if (MACHINE_ENDIANNESS == 'LITTLE_ENDIAN') {
        $val = strrev($val);
    }
    $stream->write($val);
}

/**
 * Encode an Erlang bit binary into the stream.
 *
 * @param BitBinary $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_bit_binary(BitBinary $data, StreamInterface $stream) {
    $stream->write(chr(77));
    $stream->write(pack('N', strlen($data->data)));
    $stream->write(chr($data->bits));
    $stream->write($data->data);
}

/**
 * Encode an Erlang atom cache ref into the stream.
 *
 * @param AtomCacheRef $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_atom_cache_ref(AtomCacheRef $data, StreamInterface $stream) {
    $stream->write(chr(82));
    $stream->write(chr($data->value));
}

/**
 * Encode a small integer (byte) into the stream.
 *
 * @param integer $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_small_integer($data, StreamInterface $stream) {
    $stream->write(chr(97));
    $stream->write(chr($data));
}

/**
 * Encode an integer into the stream.
 *
 * @param integer $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_integer($data, StreamInterface $stream) {
    $stream->write(chr(98));
    $stream->write(pack('N', $data));
}

/**
 * Encode any-size number into the stream.
 *
 * @param integer $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_number($data, StreamInterface $stream) {
    if ($data >= 0 && $data <= 0xff) {
        encode_small_integer($data, $stream);
    } elseif ($data >= -0x7fffffff - 1 && $data <= 0x7fffffff) {
        encode_integer($data, $stream);
    }
}

/**
 * Encode an Erlang atom into the stream.
 *
 * @param Atom $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_atom(Atom $data, StreamInterface $stream) {
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

/**
 * Encode an Erlang reference into the stream.
 *
 * @param Reference $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_reference(Reference $data, StreamInterface $stream) {
    $stream->write(chr(101));
    encode($data->atom, $stream, false);
    $stream->write(pack('N', $data->identifier));
    $stream->write(chr($data->creation));
}

/**
 * Encode an Erlang port into the stream.
 *
 * @param Port $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_port(Port $data, StreamInterface $stream) {
    $stream->write(chr(102));
    encode($data->atom, $stream, false);
    $stream->write(pack('N', $data->identifier));
    $stream->write(chr($data->creation));
}

/**
 * Encode an Erlang pid into the stream.
 *
 * @param Pid $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_pid(Pid $data, StreamInterface $stream) {
    $stream->write(chr(103));
    encode($data->atom, $stream, false);
    $stream->write(pack('N', $data->identifier));
    $stream->write(pack('N', $data->serial));
    $stream->write(chr($data->creation));
}

/**
 * Encode a tuple into the stream.
 *
 * @param Tuple $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_tuple(Tuple $data, StreamInterface $stream) {
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

/**
 * Encode a NoneType into the stream (as Erlang nil).
 *
 * @param null $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_null($data, StreamInterface $stream) {
    $stream->write(chr(106));
}

/**
 * Encode an Erlang binary into the stream.
 *
 * @param Binary $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_binary(Binary $data, StreamInterface $stream) {
    $stream->write(chr(109));
    $stream->write(pack('N', strlen($data->data)));
    $stream->write($data->data);
}

/**
 * Encode a string into the stream.
 *
 * @param string $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_str($data, StreamInterface $stream) {
    $data_len = strlen($data);
    if ($data_len > 0xffff) {
        $stream->write(chr(108));
        $stream->write(pack('N', $data_len));
        for ($i = 0; $i < $data_len; $i++) {
            encode(ord($data[$i]), $stream, false);
        }
        $stream->write(chr(106));
    } else {
        $stream->write(chr(107));
        $stream->write(pack('n', $data_len));
        $stream->write($data);
    }
}

/**
 * Encode a list into the stream.
 *
 * @param array $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_list(array $data, StreamInterface $stream) {
    $stream->write(chr(108));
    $stream->write(pack('N', count($data)));
    foreach ($data as $value) {
        encode($value, $stream, false);
    }
    $stream->write(chr(106));
}

/**
 * Encode an associative array into the stream (as a property list).
 *
 * @param array $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_dict(array $data, StreamInterface $stream) {
    $stream->write(chr(108));
    $stream->write(pack('N', count($data)));
    foreach ($data as $key => $value) {
        encode(new Tuple(array($key, $value)), $stream, false);
    }
    $stream->write(chr(106));
}

/**
 * Encode an array into the stream (list or associative).
 *
 * The way that an array is deemed to be a list is its keys are compared
 * to the range of numbers represented by [0..C] where C is the size of
 * the array.
 *
 * @param array $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_array($data, StreamInterface $stream) {
    if (array_keys($data) === range(0, count($data) - 1)) {
        encode_list($data, $stream);
    } else {
        encode_dict($data, $stream);
    }
}

/**
 * Encode an Erlang new reference into the stream.
 *
 * @param NewReference $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_new_reference(NewReference $data, StreamInterface $stream) {
    $stream->write(chr(114));
    $ids_len = count($data->ids);
    $stream->write(pack('n', $ids_len));
    encode($data->atom, $stream, false);
    $stream->write(chr($data->creation));
    foreach ($data->ids as $id) {
        $stream->write(pack('N', $id));
    }
}

/**
 * Encode an Erlang function into the stream.
 *
 * @param ErlFunction $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_function(ErlFunction $data, StreamInterface $stream) {
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

/**
 * Encode an Erlang "new function" into the stream.
 *
 * @param NewFunction $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_new_function(NewFunction $data, StreamInterface $stream) {
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

/**
 * Encode an Erlang export into the stream.
 *
 * @param Export $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode_export(Export $data, StreamInterface $stream) {
    $stream->write(chr(113));
    encode($data->module, $stream, false);
    encode($data->function, $stream, false);
    encode($data->arity, $stream, false);
}

/**
 * Encode the given data into the given stream.
 *
 * If send_magic_byte, the value 131 is sent before anything (this is
 * how Erlang denotes that there is a new piece of data coming across).
 * However, for nested data, this only needs to be sent once, hence
 * the optional argument.
 *
 * @param mixed $data
 * @param StreamInterface $stream
 *
 * @return void
 */
function encode($data, StreamInterface $stream, $send_magic_byte=true) {
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
    elseif ($data instanceof Serializable) { encode($data->toErlang(), $stream, false); }
    else {
        throw new Exception(get_class($data) . ' is not Erlang serializable');
    }
}

/**
 * Transform an array of bytes into a binary string.
 *
 * @param array $input
 *
 * @return string
 */
function to_binary($input) {
    $output = '';
    foreach ($input as $val) {
        $output .= chr($val);
    }
    return $output;
}

/**
 * Transform a binary string into an array of bytes.
 *
 * @param string $input
 *
 * @return array
 */
function from_binary($input) {
    $output = array();
    for ($i = 0; $i < strlen($input); $i++) {
        $output[] = ord($input[$i]);
    }
    return $output;
}

/**
 *
 */
class Gateway {
    /**
     * The raw device stream that to read/write to.
     *
     * @param StreamInterface
     */
    private $stream;

    /**
     * The stream wrapper that is used to abstract away the raw stream.
     *
     * @param StreamInterface
     */
    private $stream_wrapper;

    /**
     * Initialize with an optional stream. If no stream is provided,
     * Standard I/O will be used.
     *
     * @param StreamInterface $stream
     *
     * @return void
     */
    public function __construct(StreamInterface $stream=null) {
        if ($stream) {
            $this->setStream($stream);
        } else {
            $this->setStream(new StdioWrapper());
        }
        $this->stream_wrapper = new StreamEmulator();
    }

    /**
     * Close any open stream and set the new one.
     *
     * @param StreamInterface $stream
     *
     * @return void
     */
    public function setStream(StreamInterface $stream) {
        $this->close();
        $this->stream = $stream;
    }

    /**
     * If there is an active stream, close it.
     *
     * @return void
     */
    private function close() {
        if ($this->stream) {
            $this->stream->close();
        }
    }

    /**
     * Receive one message from Hurricane.
     *
     * @return mixed
     */
    public function recv() {
        $message_len = $this->stream->read(4);

        if (strlen($message_len) < 4) {
            throw new Exception('Message size payload should be 4 bytes');
        }

        $message_len = reset(unpack('N', $message_len));
        $this->stream_wrapper->clear();
        $this->stream_wrapper->write($this->stream->read($message_len));
        $message = decode($this->stream_wrapper);
        return $message;
    }

    /**
     * Send one message to Hurricane.
     *
     * @param mixed $message
     *
     * @return void
     */
    public function send($message) {
        $this->stream_wrapper->clear();
        encode($message, $this->stream_wrapper);
        $this->stream->write(pack('N', strlen($this->stream_wrapper->data)));
        $this->stream->write($this->stream_wrapper->data);
        $this->stream->flush();
    }
}
