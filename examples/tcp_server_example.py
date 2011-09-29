#!/usr/bin/env python

import os
import sys
sys.path.append(
    os.path.join(
            os.path.dirname(
                os.path.dirname(
                    os.path.abspath(__file__))),
    'drivers/python'))
from erl_codec import Gateway, Atom, SocketWrapper
from datetime import datetime

def main():
    gateway = Gateway(SocketWrapper('localhost', 3307))
    gateway.send((Atom('register_with_group'), Atom('time_server')))
    while True:
        type, src, tag, data = gateway.recv()
        if type.name == 'request':
            gateway.send((Atom('response'), src, tag, str(datetime.now())))

if __name__ == '__main__':
    main()
