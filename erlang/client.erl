-module(client).

-import(hex).
-import(erlsha2).

-export([verify/2, find_nonce/1]).

verify(Input, Nonce) ->
  Hash = erlsha2:sha256_update(erlsha2:sha256_init(), Input ++ Nonce),
  Digest = hex:bin_to_hexstr(erlsha2:sha256_final(Hash)).

find_nonce(Input) ->
  Nonce = 0,
  verify(Input, Nonce).
  string:sub_string(Digest, 63, 64).