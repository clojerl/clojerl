-module('clojerl.Var.clojerl.Stringable').

-behaviour('clojerl.Stringable').

-export([str/1]).

str({_, #{ns := NsSym, name := NameSym}}) ->
  <<(clj_core:str(NsSym))/binary
    , "/"
    , (clj_core:str(NameSym))/binary>>.
