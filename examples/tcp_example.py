#!/usr/bin/env python

from hurricane import Gateway, Message
from hurricane.erl_codec import SocketWrapper

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
