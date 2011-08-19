<?php

require dirname(__FILE__) . '/../drivers/php/erl_codec.php';

date_default_timezone_set('America/Denver');

$gateway = new Erlang\Gateway();
$gateway->send(new Erlang\Tuple(array(new Erlang\Atom('ready'))));

while (true) {
    $message = $gateway->recv();
    $message->data[0] = new Erlang\Atom('response');
    $message->data[3] = date('Y-m-d H:i:s');
    $gateway->send($message);
}
