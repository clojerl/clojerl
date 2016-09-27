-module('clojerl.reader.ReaderConditional').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').

-export([?CONSTRUCTOR/2]).
-export([equiv/2]).

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
