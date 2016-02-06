-module('clojerl.reader.ReaderConditional').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').

-export([new/2]).
-export(['clojerl.IEquiv.equiv'/2]).

-type type() :: #?TYPE{}.

-spec new('clojerl.List':type(), boolean()) -> type(). 
new(List, IsSplicing) ->
  #?TYPE{data = {List, IsSplicing}}.

'clojerl.IEquiv.equiv'( #?TYPE{name = ?M, data = {X1, Y}}
                      , #?TYPE{name = ?M, data = {X2, Y}}
                      ) ->
  clj_core:equiv(X1, X2);
'clojerl.IEquiv.equiv'(_, _) ->
  false.
