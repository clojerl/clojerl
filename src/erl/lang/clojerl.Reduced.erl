-module('clojerl.Reduced').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([deref/1]).
-export([hash/1]).
-export([str/1]).

-export([?CONSTRUCTOR/1]).
-export([is_reduced/1]).

-type type() :: #{ ?TYPE => ?M
                 , value => any()
                 }.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  #{?TYPE => ?M, value => Value}.

-spec is_reduced(type()) -> boolean().
is_reduced(#{?TYPE := ?M}) -> true;
is_reduced(_) -> false.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

deref(#{?TYPE := ?M, value := Value}) -> Value.

hash(#{?TYPE := ?M} = X) -> erlang:phash2(X).

str(#{?TYPE := ?M, value := Value}) ->
  ValueStr = clj_rt:str(Value),
  <<"#<clojerl.Reduced ", ValueStr/binary, ">">>.
