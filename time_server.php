<?php

require 'erl_codec.php';

date_default_timezone_set('America/Denver');

$gateway = Erlang\Gateway::getInstance();
while (true) {
    $message = $gateway->recv();
    file_put_contents('/Users/icheishvili/Desktop/log.txt', print_r($message, true), FILE_APPEND);
    $message->data[0] = new Erlang\Atom('response');
    $message->data[3] = date('Y-m-d H:i:s');
    $gateway->send($message);
}
