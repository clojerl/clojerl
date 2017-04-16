-module('clojerl.Symbol').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IFn').
-behavior('clojerl.IHash').
-behavior('clojerl.IMeta').
-behavior('clojerl.Named').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/1, ?CONSTRUCTOR/2]).

-export([equiv/2]).
-export([apply/2]).
-export([hash/1]).
-export([ meta/1
        , with_meta/2
        ]).
-export([ name/1
        , namespace/1
        ]).
-export([str/1]).

-type type() :: #?TYPE{data :: {?NIL | binary(), binary()}}.

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(Name) when is_binary(Name) ->
  ?CONSTRUCTOR(?NIL, Name).

-spec ?CONSTRUCTOR(binary() | ?NIL, binary()) -> type().
?CONSTRUCTOR(Namespace, Name)
  when is_binary(Namespace) orelse Namespace == ?NIL,
       is_binary(Name) ->
  #?TYPE{data = {Namespace, Name}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IFn

apply(#?TYPE{name = ?M} = Symbol, [Map]) ->
  clj_rt:get(Map, Symbol);
apply(#?TYPE{name = ?M} = Symbol, [Map, NotFound]) ->
  clj_rt:get(Map, Symbol, NotFound);
apply(_, Args) ->
  CountBin = integer_to_binary(length(Args)),
  throw(<<"Wrong number of args for symbol, got: ", CountBin/binary>>).

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = {?NIL, Name}}) ->
  Name;
str(#?TYPE{name = ?M, data = {Namespace, Name}}) ->
  <<Namespace/binary, "/", Name/binary>>.

name(#?TYPE{name = ?M, data = {_, Name}}) -> Name.

namespace(#?TYPE{name = ?M, data = {Namespace, _}}) ->
  Namespace.

meta(#?TYPE{name = ?M, info = Info}) ->
  maps:get(meta, Info, ?NIL).

with_meta( #?TYPE{name = ?M, info = Info} = Keyword
                         , Metadata
                         ) ->
  Keyword#?TYPE{info = Info#{meta => Metadata}}.

equiv( #?TYPE{name = ?M, data = X}
                      , #?TYPE{name = ?M, data = X}
                      ) ->
  true;
equiv(_, _) ->
  false.

hash(#?TYPE{name = ?M, data = Data}) ->
  erlang:phash2(Data).
