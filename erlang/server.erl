-module(server).
-export([start/0, start/1]).

-import(hex).
start() ->
  start(1337).
start(Port) ->
  spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
                  loop(Sock) end).

loop(Sock) ->
  {ok, Conn} = gen_tcp:accept(Sock),
  Handler = spawn(fun () -> handle(Conn) end),
  gen_tcp:controlling_process(Conn, Handler),
  loop(Sock).

handle(Conn) ->
  gen_tcp:send(Conn, "ok\n"),
  receive_and_work(Conn).


receive_and_work(Conn) ->
  case gen_tcp:recv(Conn, 0, 360 * 1000) of
    {ok, Packet} ->
      gen_tcp:send(Conn, [ Packet | [ ":" | response(trim_whitespace(Packet))]]),
      gen_tcp:close(Conn);
    {error, Reason} ->
      io:format("Error happened ~p~n", Reason)
  end.

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

trim_whitespace(Input) ->
  re:replace(Input, "\\s+", "", [global]).