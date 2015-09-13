-module('clojerl.List.clojerl.IMeta').

-behavior('clojerl.IMeta').

-export([meta/1, with_meta/2]).

-spec meta('clojerl.List':type()) -> any().
meta({'clojerl.List', _Data}) ->
  undefined.

-spec with_meta(any(), 'clojerl.Map':type()) -> any().
with_meta({'clojerl.List', Data}, _Metadata) ->
  {'clojerl.List', Data}.
