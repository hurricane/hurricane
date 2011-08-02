<?php

require dirname(__FILE__) . '/../drivers/php/erl_codec.php';

date_default_timezone_set('America/Denver');

$gateway = new Erlang\Gateway(new Erlang\SocketWrapper('localhost', '3307'));
while (true) {
    $gateway->send(
        new Erlang\Tuple(
            array(
                new Erlang\Atom('request'),
                new Erlang\Atom('time_server'),
                new Erlang\Atom('time_message'),
                null
            )
        )
    );
    $response = $gateway->recv();
    echo $response->data[3] . PHP_EOL;
}
