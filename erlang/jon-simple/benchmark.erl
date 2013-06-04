-module(benchmark).
-export([start/0]).

start() ->
    crypto:start(),
    spawn(fun() -> {ok, Listen} = gen_tcp:listen(1337, [binary,
                                                        {reuseaddr, true},
                                                        {backlog, 100},
                                                        {active, false}]),
                   connect(Listen) end).

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    Pid = spawn(fun() -> handle(Socket) end),
    gen_tcp:controlling_process(Socket, Pid),
    gen_tcp:send(Socket, "ok\n"),
    connect(Listen).

handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            Str = binary_to_list(Data),
            Reply = lists:concat([Str, ":", find_solution(Str)]),
            gen_tcp:send(Socket, Reply),
            handle(Socket);
        {tcp_error, Socket, Reason} ->
            io:format("error: ~p~n", [Reason]);
        {tcp_closed, Socket} ->
            done
    end.

find_solution(Input)    -> find_solution(Input, 0).
find_solution(Input, N) ->
    Hex = string:to_lower(integer_to_list(N, 16)),
    case verify(Input, Hex) of
        true  -> Hex;
        false -> find_solution(Input, N + 1)
    end.

verify(Input, Nonce) ->
    Hash = crypto:hash(sha256, lists:concat([Input, Nonce])),
    binary:last(Hash) == 0.
