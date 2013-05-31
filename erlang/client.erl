-module(client).

-import(hex).

-export([verify/2, find_nonce/2]).

verify(Input, Nonce) ->
  hex:bin_to_hexstr(crypto:hash(sha256, [Input | string:to_lower(integer_to_list(Nonce, 16))])).

find_nonce(Input, Nonce) ->
  Digest = verify(Input, Nonce),
  case string:right(Digest, 4) == "0000" of
    true -> string:to_lower(integer_to_list(Nonce, 16));
    false -> find_nonce(Input, Nonce + 1)
  end.
