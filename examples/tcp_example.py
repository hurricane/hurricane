#!/usr/bin/env python

import os
import sys
sys.path.append(
    os.path.join(
        os.path.dirname(
            os.path.dirname(
                os.path.abspath(__file__))),
    'drivers/python'))
from erl_codec import SocketWrapper
from hurricane import Gateway, Message

def main():
    gateway = Gateway(SocketWrapper('localhost', 3000))
    while True:
        message = Message()
        message.type = 'request'
        message.destination = 'time_server'
        message.tag = 0
        message.data = None
        gateway.send(message)
        print gateway.recv()

if __name__ == '__main__':
    main()
