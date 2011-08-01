#!/usr/bin/env python

from erl_codec import Gateway, Atom

def main():
    gateway = Gateway()
    while True:
        source, data = gateway.recv()
        data += 1
        gateway.send((Atom('php_echo'), data))

if __name__ == '__main__':
    main()
