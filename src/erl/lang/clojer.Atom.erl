-module('clojer.Atom').

-include("clojerl.hrl").

-behavior('clojerl.IDeref').
-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/1]).

-export([deref/1]).
-export([equiv/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([str/1]).

-type type() :: #?TYPE{data :: {binary(), any()}}.

-spec ?CONSTRUCTOR(any()) -> type().
?CONSTRUCTOR(Value) ->
  UUID = 'erlang.util.UUID':random(),
  #?TYPE{data = {UUID, Value}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

str(#?TYPE{name = ?M, data = {UUID, _}}) ->
  UUIDStr = 'erlang.util.UUID':str(UUID),
  <<"#<clojerl.Atom ", UUIDStr/binary, ">">>.

deref(#?TYPE{name = ?M, data = {_UUID, _Value}} = _Atom) ->
  ok.

equiv(_, _) ->
  false.

hash(#?TYPE{name = ?M, data = {UUID, _}}) ->
  erlang:phash2(UUID).

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta( #?TYPE{name = ?M, info = Info} = Keyword
         , Metadata
         ) ->
  Keyword#?TYPE{info = Info#{meta => Metadata}}.
