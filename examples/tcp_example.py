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
import re

def main():
    s = SocketWrapper('localhost', 3307)
    gateway = Gateway(s)
    while True:
        gateway.send((Atom('request'), Atom('time_server'), Atom('time_message'), None))
        print gateway.recv()

if __name__ == '__main__':
    main()
