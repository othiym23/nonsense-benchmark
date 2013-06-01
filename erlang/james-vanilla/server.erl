%
% My attempt to learn Erlang and get something working - James Hwang
%

-module(server).
-export([start/0, start/1]).

% Import a small hex module to help with the binary to hex string
-import(hex).

% Either start the server by default on port 1337 or user defined.
% TODO - "gen_tcp" and its limitations might be causing the packets to drop
start() ->
  start(1337).
start(Port) ->
  spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
                  loop(Sock) end).

% The loop that handles all the incoming data from the socket
loop(Sock) ->
  {ok, Conn} = gen_tcp:accept(Sock),
  Handler = spawn(fun () -> handle(Conn) end),
  gen_tcp:controlling_process(Conn, Handler),
  loop(Sock).

% After recieving a connection, send an ok
% TODO - could be combined with receive_and_work function
handle(Conn) ->
  gen_tcp:send(Conn, "ok\n"),
  receive_and_work(Conn).

% Receive input from the connection and call response
receive_and_work(Conn) ->
  case gen_tcp:recv(Conn, 0, 360 * 1000) of
    {ok, Packet} ->
      gen_tcp:send(Conn, [ Packet | [ ":" | response(trim_whitespace(Packet))]]),
      gen_tcp:close(Conn);
    {error, Reason} ->
      io:format("Error happened ~p~n", Reason)
  end.

% Where the work of the SHA and iteration is happening
response(Input) ->
  find_nonce(Input, 0).

verify(Input, Nonce) ->
  hex:bin_to_hexstr(crypto:hash(sha256, [Input | string:to_lower(integer_to_list(Nonce, 16))])).

find_nonce(Input, Nonce) ->
  Digest = verify(Input, Nonce),
  case string:right(Digest, 2) == "00" of
    true -> string:to_lower(integer_to_list(Nonce, 16));
    false -> find_nonce(Input, Nonce + 1)
  end.

% helper function
trim_whitespace(Input) ->
  re:replace(Input, "\\s+", "", [global]).