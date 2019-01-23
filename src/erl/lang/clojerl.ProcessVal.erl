-module('clojerl.ProcessVal').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([ ?CONSTRUCTOR/1
        , reset/2
        , destroy/1
        ]).

-export([deref/1]).
-export([equiv/2]).
-export([hash/1]).
-export([str/1]).

-type type() :: #{ ?TYPE => ?M
                 , id    => binary()
                 }.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  UUID = 'erlang.util.UUID':random(),
  Id   = 'erlang.util.UUID':str(UUID),
  _    = erlang:put(Id, {ok, Value}),
  #{ ?TYPE => ?M
   , id    => Id
   }.

-spec reset(type(), any()) -> any().
reset(#{?TYPE := ?M, id := Id}, Value) ->
  _ = erlang:put(Id, {ok, Value}),
  Value.

-spec destroy(type()) -> ok.
destroy(#{?TYPE := ?M, id := Id}) ->
  erlang:erase(Id),
  ok.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(#{?TYPE := ?M, id := Id}) ->
  <<"#<clojerl.ProcessVal ", Id/binary, ">">>.

deref(#{?TYPE := ?M, id := Id}) ->
  case erlang:get(Id) of
    undefined   -> ?ERROR([<<"No process value available for: ">>, Id]);
    {ok, Value} -> Value
  end.

equiv( #{?TYPE := ?M, id := Id}
     , #{?TYPE := ?M, id := Id}
     ) ->
  true;
equiv(_, _) ->
  false.

hash(#{?TYPE := ?M, id := Id}) ->
  erlang:phash2(Id).
