<?php

require 'erl_codec.php';

$gateway = Erlang\Gateway::getInstance();
while (true) {
    $message = $gateway->recv();
    $message->data[0] = new Erlang\Atom('py_echo');
    $message->data[1] += 1;
    $gateway->send($message);
}
