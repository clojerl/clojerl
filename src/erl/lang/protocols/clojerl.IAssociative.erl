-module('clojerl.IAssociative').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['contains_key'/2, 'entry_at'/2, 'assoc'/3, '__satisfies?__'/1]).

-callback 'contains_key'(any(), any()) -> any().
-callback 'entry_at'(any(), any()) -> any().
-callback 'assoc'(any(), any(), any()) -> any().

'contains_key'(Assoc, Key) ->
  case clj_rt:type_module(Assoc) of
    'erlang.Map' ->
      'erlang.Map':'contains_key'(Assoc, Key);
    'clojerl.Map' ->
      'clojerl.Map':'contains_key'(Assoc, Key);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'contains_key'(Assoc, Key);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'contains_key'(Assoc, Key);
    'clojerl.Vector' ->
      'clojerl.Vector':'contains_key'(Assoc, Key);
    _ ->
      clj_protocol:resolve(?MODULE, 'contains_key', Assoc, Key)
  end.

'entry_at'(Assoc, Key) ->
  case clj_rt:type_module(Assoc) of
    'erlang.Map' ->
      'erlang.Map':'entry_at'(Assoc, Key);
    'clojerl.Map' ->
      'clojerl.Map':'entry_at'(Assoc, Key);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'entry_at'(Assoc, Key);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'entry_at'(Assoc, Key);
    'clojerl.Vector' ->
      'clojerl.Vector':'entry_at'(Assoc, Key);
    _ ->
      clj_protocol:resolve(?MODULE, 'entry_at', Assoc, Key)
  end.

'assoc'(Assoc, Key, Value) ->
  case clj_rt:type_module(Assoc) of
    'erlang.Map' ->
      'erlang.Map':'assoc'(Assoc, Key, Value);
    'clojerl.Map' ->
      'clojerl.Map':'assoc'(Assoc, Key, Value);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'assoc'(Assoc, Key, Value);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'assoc'(Assoc, Key, Value);
    'clojerl.Vector' ->
      'clojerl.Vector':'assoc'(Assoc, Key, Value);
    _ ->
      clj_protocol:resolve(?MODULE, 'assoc', Assoc, Key, Value)
  end.

?SATISFIES('erlang.Map') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES(_) -> false.
