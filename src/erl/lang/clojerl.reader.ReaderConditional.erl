%% @private
-module('clojerl.reader.ReaderConditional').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IHash').
-behavior('clojerl.ILookup').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/2]).

-export([equiv/2]).
-export([hash/1]).
-export([ get/2
        , get/3
        ]).
-export([str/1]).

-type type() :: #{ ?TYPE       => ?M
                 , list        => any()
                 , is_splicing => boolean()
                 }.

-spec ?CONSTRUCTOR('clojerl.List':type(), boolean()) -> type().
?CONSTRUCTOR(List, IsSplicing) ->
  #{ ?TYPE       => ?M
   , list        => List
   , is_splicing => IsSplicing
   }.

equiv( #{?TYPE := ?M, list := X1, is_splicing := Y}
     , #{?TYPE := ?M, list := X2, is_splicing := Y}
     ) ->
  clj_rt:equiv(X1, X2);
equiv(_, _) ->
  false.

%% clojerl.IHash

hash(#{?TYPE := ?M} = ReaderCond) ->
  erlang:phash2(ReaderCond).

%% clojerl.ILookup

get(#{?TYPE := ?M} = ReaderCond, Key) ->
  get(ReaderCond, Key, ?NIL).

get(#{?TYPE := ?M, list := Form}, form, _) ->
  Form;
get(#{?TYPE := ?M, is_splicing := IsSplicing}, 'splicing?', _) ->
  IsSplicing;
get(#{?TYPE := ?M}, _, NotFound) ->
  NotFound.

%% clojerl.IStringable

str(#{?TYPE := ?M, list := List, is_splicing := IsSplicing}) ->
  Splice = case IsSplicing of
             true  -> <<"@">>;
             false -> <<>>
           end,
  ListBin = clj_rt:str(List),
  <<"#?", Splice/binary, ListBin/binary>>.
