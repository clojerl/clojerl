-module('clojerl.IFn').

-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['apply'/2]).
-export([?SATISFIES/1]).

-callback 'apply'(any(), any()) -> any().

'apply'(Fn, Args) ->
  case clj_rt:type_module(Fn) of
    'erlang.Map' ->
      'erlang.Map':'apply'(Fn, Args);
    'erlang.Fn' ->
      'erlang.Fn':'apply'(Fn, Args);
    'clojerl.Var' ->
      'clojerl.Var':'apply'(Fn, Args);
    'clojerl.Keyword' ->
      'clojerl.Keyword':'apply'(Fn, Args);
    'clojerl.Symbol' ->
      'clojerl.Symbol':'apply'(Fn, Args);
    'clojerl.SortedMap' ->
      'clojerl.SortedMap':'apply'(Fn, Args);
    'clojerl.TupleMap' ->
      'clojerl.TupleMap':'apply'(Fn, Args);
    'clojerl.Vector' ->
      'clojerl.Vector':'apply'(Fn, Args);
    'clojerl.Map' ->
      'clojerl.Map':'apply'(Fn, Args);
    'clojerl.Set' ->
      'clojerl.Set':'apply'(Fn, Args);
    'clojerl.SortedSet' ->
      'clojerl.SortedSet':'apply'(Fn, Args);
    Type ->
      clj_protocol:not_implemented(?MODULE, 'apply', Type)
  end.

?SATISFIES('erlang.Map') -> true;
?SATISFIES('erlang.Fn') -> true;
?SATISFIES('clojerl.Var') -> true;
?SATISFIES('clojerl.Keyword') -> true;
?SATISFIES('clojerl.Symbol') -> true;
?SATISFIES('clojerl.SortedMap') -> true;
?SATISFIES('clojerl.TupleMap') -> true;
?SATISFIES('clojerl.Vector') -> true;
?SATISFIES('clojerl.Map') -> true;
?SATISFIES('clojerl.Set') -> true;
?SATISFIES('clojerl.SortedSet') -> true;
?SATISFIES(_) -> false.
