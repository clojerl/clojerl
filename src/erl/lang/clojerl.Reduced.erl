-module('clojerl.Reduced').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export([deref/1]).
-export([hash/1]).
-export([str/1]).

-export([?CONSTRUCTOR/1]).
-export([is_reduced/1]).

-type type() :: #?TYPE{data :: any()}.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  #?TYPE{data = Value}.

-spec is_reduced(type()) -> boolean().
is_reduced(#?TYPE{name = ?M}) -> true;
is_reduced(_) -> false.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

deref(#?TYPE{name = ?M, data = Value}) -> Value.

hash(#?TYPE{name = ?M} = X) -> erlang:phash2(X).

str(#?TYPE{name = ?M, data = Value}) ->
  ValueStr = clj_rt:str(Value),
  <<"#<clojerl.Reduced ", ValueStr/binary, ">">>.
