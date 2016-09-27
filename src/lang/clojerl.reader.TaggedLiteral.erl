-module('clojerl.reader.TaggedLiteral').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.ILookup').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/2]).
-export([equiv/2]).
-export([ get/2
        , get/3
        ]).
-export([str/1]).

-type type() :: #?TYPE{}.

-spec ?CONSTRUCTOR('clojerl.Symbol':type(), any()) -> type().
?CONSTRUCTOR(Tag, Form) ->
  #?TYPE{data = {Tag, Form}}.

equiv( #?TYPE{name = ?M, data = {T1, F1}}
     , #?TYPE{name = ?M, data = {T2, F2}}
     ) ->
  clj_core:equiv(T1, T2) andalso clj_core:equiv(F1, F2);
equiv(_, _) ->
  false.

%% clojerl.ILookup

get(#?TYPE{name = ?M} = TaggedLiteral, Key) ->
  get(TaggedLiteral, Key, undefined).

get(#?TYPE{name = ?M, data = {Tag, _}}, tag, _) ->
  Tag;
get(#?TYPE{name = ?M, data = {_, Form}}, form, _) ->
  Form;
get(#?TYPE{name = ?M}, _, NotFound) ->
  NotFound.

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = {Tag, Form}}) ->
  TagBin = clj_core:str(Tag),
  FormBin = clj_core:str(Form),
  <<"#", TagBin/binary, " ", FormBin/binary>>.
