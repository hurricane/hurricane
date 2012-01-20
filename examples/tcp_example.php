<?php

require dirname(__FILE__) . '/../drivers/php/hurricane.php';

date_default_timezone_set('America/Denver');

$gateway = new \Hurricane\Gateway(
    new \Erlang\SocketWrapper('localhost', '3000')
);
while (true) {
    $request = \Hurricane\Message::create()
        ->setType('request')
        ->setDestination('time_server')
        ->setTag(0)
        ->setData(null);
    $gateway->send($request);
    $response = $gateway->recv();
    echo $response->getData() . PHP_EOL;
}
