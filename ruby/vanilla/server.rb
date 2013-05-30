#!/usr/bin/env ruby

require 'socket'
require 'digest'

module Nonsense
  class Server
    def initialize(port)
      @port = port
      @server = TCPServer.new(port)
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

    def run
      puts "Starting sever on port #{@port}"
      loop do
        client = @server.accept
        client.write "ok\n"
        buf = client.readpartial(1024)
        nonce = compute_nonce(buf)
        client.write "#{buf}:#{nonce}"
        client.close
      end
    end
  end
end

server = Nonsense::Server.new(1337)
server.run
