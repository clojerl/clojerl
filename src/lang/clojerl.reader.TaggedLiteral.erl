-module('clojerl.reader.TaggedLiteral').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').

-export([?CONSTRUCTOR/2]).
-export([equiv/2]).

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
