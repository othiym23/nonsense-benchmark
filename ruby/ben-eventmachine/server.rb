#!/usr/bin/env ruby

require 'socket'
require 'digest'
require 'eventmachine'

module Nonsense
  class Server < EventMachine::Connection
    def post_init
      self.send_data("ok\n")
    end

    def compute_nonce(msg)
      i = 0
      nonce = nil
      digest = Digest::SHA256.new
      loop do
        nonce = i.to_s(16)
        return nonce if digest.reset.update(msg).update(nonce).digest[-1] == "\0"
        i += 1
      end
    end

    def receive_data(data)
      nonce = compute_nonce(data)
      send_data("#{data}:#{nonce}")
      close_connection_after_writing
    end
  end
end

EM.run do
  EM.start_server '0.0.0.0', 1337, Nonsense::Server
end