-module('clj.example.gen_server.init__var').

-export([val/0]).

val() ->
  fun([X]) -> init_1(X);
     (_) -> throw(wrong_num_args)
  end.

init_1(_X) ->
  {ok, #{}}.
