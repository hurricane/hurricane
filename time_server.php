<?php

require 'erl_codec.php';

date_default_timezone_set('America/Denver');

$gateway = Erlang\Gateway::getInstance();
while (true) {
    $message = $gateway->recv();
    $message->data[0] = new Erlang\Atom('response');
    $message->data[3] = date('Y-m-d H:i:s');
    $gateway->send($message);
}
