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
loop do
  request = Hurricane::Message.new()
  request.type = 'request'
  request.destination = 'time_server'
  request.tag = 0
  request.data = nil

  gateway.do_send(request)
  puts gateway.do_recv().inspect()
end
