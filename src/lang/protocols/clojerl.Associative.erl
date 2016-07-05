-module('clojerl.Associative').

-export([contains_key/2, entry_at/2, assoc/3]).

-type type() :: any().

-callback contains_key(A :: type(), K :: any()) -> boolean().

-callback entry_at(A :: type(), K :: any()) -> any().

-callback assoc(A :: type(), K :: any(), V :: any()) -> any().

-spec contains_key(type(), any()) -> boolean().
contains_key(Assoc, Key) ->
  'clojerl.protocol':resolve(?MODULE, contains_key, [Assoc, Key]).

-spec entry_at(type(), any()) -> boolean().
entry_at(Assoc, Key) ->
  'clojerl.protocol':resolve(?MODULE, entry_at, [Assoc, Key]).

-spec assoc(type(), any(), any()) -> boolean().
assoc(Assoc, Key, Value) ->
  'clojerl.protocol':resolve(?MODULE, assoc, [Assoc, Key, Value]).
