#!/usr/bin/env python

from datetime import datetime
from hurricane import Gateway, Message
from hurricane.erl_codec import SocketWrapper, Atom

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
