#!/usr/bin/env python

import os
import re
import sys
sys.path.append(
    os.path.join(
        os.path.dirname(
            os.path.dirname(
                os.path.abspath(__file__))),
    'drivers/python'))
from erl_codec import SocketWrapper, Atom
from hurricane import Gateway, Message

def parse_http_request(data):
    parsed_data = {}

    for datum in data:
        key = datum[0]
        value = datum[1]
        if type(key) == Atom:
            key = key.name
        if type(value) == Atom:
            value = value.name
        elif key == 'headers' and value is not None:
            value = parse_http_request(value)
        elif key == 'params' and value is not None:
            value = parse_http_request(value)
        parsed_data[key] = value

    return parsed_data

def main():
    gateway = Gateway()
    gateway.send_ready_signal()

    while True:
        http_request_message = gateway.recv()
        request = parse_http_request(http_request_message.data)

        http_response_message = Message()
        http_response_message.type = 'response'
        http_response_message.destination = \
            http_request_message.destination
        http_response_message.tag = http_request_message.tag

        normalized_path = re.sub(r'/*$', '', request['path'])
        if normalized_path == '/current_time':
            time_request_message = Message()
            time_request_message.type = 'request'
            time_request_message.destination = 'time_server'
            time_request_message.tag = 'time_message'
            time_request_message.data = None
            gateway.send(time_request_message)
            gateway.send(time_request_message)
            gateway.send(time_request_message)
            gateway.send(time_request_message)
            gateway.send(time_request_message)

            time_response_message = gateway.recv()
            time_response_message = gateway.recv()
            time_response_message = gateway.recv()
            time_response_message = gateway.recv()
            time_response_message = gateway.recv()

            http_response_message.data = (
                200, [], time_response_message.data)
        else:
            http_response_message.data = (200, [], normalized_path)
        gateway.send(http_response_message)

if __name__ == '__main__':
    main()
