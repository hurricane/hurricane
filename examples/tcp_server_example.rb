#!/usr/bin/env ruby

require 'hurricane'

gateway = Hurricane::Gateway.new(
  Erlang::SocketWrapper.new('localhost', 3000))
gateway.register_server('time_server')
loop do
  request = gateway.do_recv()

  response = Hurricane::Message.new()
  response.type = 'response'
  response.destination = request.destination
  response.tag = request.tag
  response.data = Time.new.inspect()

  gateway.do_send(response)
end
