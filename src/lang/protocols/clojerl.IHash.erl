-module('clojerl.IHash').

-export([hash/1]).

-type type() ::  any().

-callback 'clojerl.IHash.hash'(any()) -> integer().

-spec hash(type()) -> any().
hash(X) ->
  'clojerl.protocol':resolve(?MODULE, hash, [X]).
