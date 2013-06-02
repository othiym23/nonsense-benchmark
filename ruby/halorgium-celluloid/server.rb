#!/usr/bin/env ruby

require 'celluloid/io'
require 'digest'

module Nonsense
  class Server
    include Celluloid::IO

    finalizer :cleanup

    def initialize(port)
      @port = port
      # TODO: fix TCPServer to support only port argument
      @server = TCPServer.new('0.0.0.0', port)

      async.run
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
      puts "Starting server on port #{@port}"
      loop do
        async.handle @server.accept
      end
    end

    def handle(client)
      client.write "ok\n"
      buf = client.readpartial(1024)
      nonce = compute_nonce(buf)
      client.write "#{buf}:#{nonce}"
      client.close
    end

    def cleanup
      @server.close if @server
    end
  end
end

server = Nonsense::Server.new(1337)
Celluloid::Actor.join(server)
