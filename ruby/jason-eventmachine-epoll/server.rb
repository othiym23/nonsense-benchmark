#!/usr/bin/env ruby

#
# Ruby + EventMachine + epoll by Jason Snell <jason@newrelic.com>
#
# This seems to run fastest with JRuby.
# 
# This will require the EventMachine gem.  You can install it with:
# $ gem install eventmachine
# 
# Run simply with:
# ./server.rb
#

require 'rubygems'
require 'eventmachine'
require 'digest/sha2'

PORT   = 1337
OK     = "ok\n"
TARGET = "00"
COLON  = ":"

EM.epoll

# This will get us a few more open filehandles.  The OS will limit this to
# whatever you set ulimit -n <x> to.
puts "descriptor-table size is #{EM.set_descriptor_table_size(60000)}"

module Prover
  def work(input)
    nonce = 0
    while nonce += 1 do
      hash = Digest::SHA2.new << input << nonce.to_s(16)
      return nonce.to_s(16) if hash.to_s[-2..-1] == TARGET
    end
  end

  def post_init
    send_data OK
  end

  def receive_data(data)
    send_data data + COLON + work(data)
    close_connection_after_writing
  end
end

EM.run do
  EM.start_server("0", PORT, Prover)
end

