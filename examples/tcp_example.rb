#!/usr/bin/env ruby

$:.push(File.join(File.dirname(File.dirname(File.expand_path(__FILE__))), 'drivers', 'ruby'))
require 'erl_codec'

s = Erlang::SocketWrapper.new('localhost', 3308)
gateway = Erlang::Gateway.new(s)
loop do
  gateway.send(Erlang::Tuple.new([Erlang::Atom.new('request'), Erlang::Atom.new('time_server'), Erlang::Atom.new('time_message'), nil]))
  puts gateway.recv().inspect()
end
