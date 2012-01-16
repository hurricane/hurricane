#!/usr/bin/env python

import os
import sys
sys.path.append(
    os.path.join(
        os.path.dirname(
            os.path.dirname(
                os.path.abspath(__file__))),
    'drivers/python'))
from erl_codec import SocketWrapper, Atom
from hurricane import Gateway, Message
from datetime import datetime

def main():
    gateway = Gateway(SocketWrapper('localhost', 3000))
    gateway.register_server('time_server')
    while True:
        request = gateway.recv()
        response = Message()
        response.type = 'response'
        response.destination = request.destination
        response.tag = request.tag
        response.data = str(datetime.now())
        gateway.send(response)

if __name__ == '__main__':
    main()
