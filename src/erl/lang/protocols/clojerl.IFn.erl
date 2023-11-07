%%% Code generate by scripts/generate-protocols
-module('clojerl.IFn').

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-clojure(true).
-protocol(true).

-export(['apply'/2]).
-export([?SATISFIES/1]).
-export([?EXTENDS/1]).

-callback 'apply'(any(), any()) -> any().
-optional_callbacks(['apply'/2]).

-export_type([type/0]).
-type type() :: #{_ => _}.

'apply'(Fn, Args) ->
  case Fn of
    #{?TYPE := 'clojerl.Fn'} ->
      'clojerl.Fn':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Map'} ->
      'clojerl.Map':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Set'} ->
      'clojerl.Set':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.SortedMap'} ->
      'clojerl.SortedMap':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.SortedSet'} ->
      'clojerl.SortedSet':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Subvec'} ->
      'clojerl.Subvec':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Symbol'} ->
      'clojerl.Symbol':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.TupleMap'} ->
      'clojerl.TupleMap':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Var'} ->
      'clojerl.Var':'apply'(Fn, Args);
    #{?TYPE := 'clojerl.Vector'} ->
      'clojerl.Vector':'apply'(Fn, Args);
    #{?TYPE := _} ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn);
    X_ when erlang:is_binary(X_) ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn);
    X_ when erlang:is_boolean(X_) ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn);
    X_ when erlang:is_map(X_) ->
      'erlang.Map':'apply'(Fn, Args);
    X_ when erlang:is_function(X_) ->
      'erlang.Fn':'apply'(Fn, Args);
    ?NIL ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn);
    X_ when erlang:is_atom(X_) ->
      'clojerl.Keyword':'apply'(Fn, Args);
    _ ->
      clj_protocol:not_implemented(?MODULE, 'apply', Fn)
  end.

?SATISFIES(X) ->
  case X of
    #{?TYPE := 'clojerl.Fn'} ->  true;
    #{?TYPE := 'clojerl.Map'} ->  true;
    #{?TYPE := 'clojerl.Set'} ->  true;
    #{?TYPE := 'clojerl.SortedMap'} ->  true;
    #{?TYPE := 'clojerl.SortedSet'} ->  true;
    #{?TYPE := 'clojerl.Subvec'} ->  true;
    #{?TYPE := 'clojerl.Symbol'} ->  true;
    #{?TYPE := 'clojerl.TupleMap'} ->  true;
    #{?TYPE := 'clojerl.Var'} ->  true;
    #{?TYPE := 'clojerl.Vector'} ->  true;
    #{?TYPE := _} ->  false;
    X_ when erlang:is_binary(X_) ->  false;
    X_ when erlang:is_boolean(X_) ->  false;
    X_ when erlang:is_map(X_) ->  true;
    X_ when erlang:is_function(X_) ->  true;
    ?NIL ->  false;
    X_ when erlang:is_atom(X_) ->  true;
    _ -> false
  end.

?EXTENDS(X) ->
  case X of
    'clojerl.Fn' -> true;
    'clojerl.Map' -> true;
    'clojerl.Set' -> true;
    'clojerl.SortedMap' -> true;
    'clojerl.SortedSet' -> true;
    'clojerl.Subvec' -> true;
    'clojerl.Symbol' -> true;
    'clojerl.TupleMap' -> true;
    'clojerl.Var' -> true;
    'clojerl.Vector' -> true;
    'erlang.Map' -> true;
    'erlang.Fn' -> true;
    'clojerl.Keyword' -> true;
    _ -> false
  end.
