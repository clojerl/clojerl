-module('clojerl.reader.ReaderConditional').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/2]).

-export([equiv/2]).
-export([hash/1]).
-export([ get/2
        , get/3
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR('clojerl.List':type(), boolean()) -> type().
?CONSTRUCTOR(List, IsSplicing) ->
  #?TYPE{data = {List, IsSplicing}}.

equiv( #?TYPE{name = ?M, data = {X1, Y}}
     , #?TYPE{name = ?M, data = {X2, Y}}
     ) ->
  clj_core:equiv(X1, X2);
equiv(_, _) ->
  false.

%% clojerl.IHash

hash(#?TYPE{name = ?M} = ReaderCond) ->
  erlang:phash2(ReaderCond).

%% clojerl.ILookup

get(#?TYPE{name = ?M} = ReaderCond, Key) ->
  get(ReaderCond, Key, ?NIL).

get(#?TYPE{name = ?M, data = {Form, _}}, form, _) ->
  Form;
get(#?TYPE{name = ?M, data = {_, IsSplicing}}, 'splicing?', _) ->
  IsSplicing;
get(#?TYPE{name = ?M}, _, NotFound) ->
  NotFound.

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = {List, IsSplicing}}) ->
  Splice = case IsSplicing of
             true  -> <<"@">>;
             false -> <<>>
           end,
  ListBin = clj_core:str(List),
  <<"#?", Splice/binary, ListBin/binary>>.
