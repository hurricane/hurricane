#!/usr/bin/env ruby

$:.push(
  File.join(
    File.dirname(File.dirname(File.expand_path(__FILE__))),
    'drivers',
    'ruby'
  )
)
require 'erl_codec'

gateway = Erlang::Gateway.new(Erlang::SocketWrapper.new('localhost', 3307))
gateway.send(
  Erlang::Tuple.new(
    [
      Erlang::Atom.new('register_with_group'),
      Erlang::Atom.new('time_server')
    ]
  )
)
loop do
  message = gateway.recv()
  gateway.send(
    Erlang::Tuple.new(
      [
        Erlang::Atom.new('response'),
        message.data[1],
        message.data[2],
        Time.new().inspect()
      ]
    )
  )
end
