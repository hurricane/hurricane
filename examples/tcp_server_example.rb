#!/usr/bin/env ruby

$:.push(
  File.join(
    File.dirname(File.dirname(File.expand_path(__FILE__))),
    'drivers',
    'ruby'
  )
)

require 'erl_codec'
require 'hurricane'

gateway = Hurricane::Gateway.new(
  Erlang::SocketWrapper.new('localhost', 3307))
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
