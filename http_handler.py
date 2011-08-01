#!/usr/bin/env python

from erl_codec import Gateway, Atom
import re

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
    while True:
        r, source, message_type, message = gateway.recv()
        request = parse_http_request(message)
        normalized_path = re.sub(r'/*$', '', request['path'])
        if normalized_path == '/current_time':
            time_message_type = Atom('time_message')
            gateway.send((Atom('request'), Atom('time_server'), time_message_type, None))
            gateway.send((Atom('request'), Atom('time_server'), time_message_type, None))
            gateway.send((Atom('request'), Atom('time_server'), time_message_type, None))
            gateway.send((Atom('request'), Atom('time_server'), time_message_type, None))
            gateway.send((Atom('request'), Atom('time_server'), time_message_type, None))
            r, time_src, time_message_type, time_message = gateway.recv()
            r, time_src, time_message_type, time_message = gateway.recv()
            r, time_src, time_message_type, time_message = gateway.recv()
            r, time_src, time_message_type, time_message = gateway.recv()
            r, time_src, time_message_type, time_message = gateway.recv()
            gateway.send((Atom('response'), source, message_type, (200, [], time_message)))
        else:
            gateway.send((Atom('response'), source, message_type, (200, [], normalized_path)))

if __name__ == '__main__':
    main()
