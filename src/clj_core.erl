-module(clj_core).

-export([
         type/1,
         count/1,
         first/1,
         next/1,
         second/1,
         third/1,
         fourth/1,
         name/1,
         namespace/1,
         symbol/1, symbol/2
        ]).

-spec count(any()) -> integer().
count(Seq) ->
  'clojerl.Counted':count(Seq).

-spec next(any()) -> any().
next(undefined) -> undefined;
next(Seq) -> 'clojerl.ISeq':next(Seq).

-spec first(any()) -> any().
first(undefined) -> undefined;
first(Seq) -> 'clojerl.ISeq':first(Seq).

-spec second(any()) -> any().
second(Seq) ->
  first(next(Seq)).

-spec third(any()) -> any().
third(Seq) ->
  first(next(next(Seq))).

-spec fourth(any()) -> any().
fourth(Seq) ->
  first(next(next(next(Seq)))).

-spec name(any()) -> any().
name(X) ->
  'clojerl.Named':name(X).

-spec namespace(any()) -> any().
namespace(X) ->
  'clojerl.Named':namespace(X).

-spec symbol(atom()) -> 'clojerl.Symbol':type().
symbol(Name) ->
  'clojerl.Symbol':new(Name).

-spec symbol(atom(), atom()) -> 'clojerl.Symbol':type().
symbol(Namespace, Name) ->
  'clojerl.Symbol':new(Namespace, Name).

-spec type(any()) -> atom().
type(X) when is_tuple(X) -> element(1, X);
type(X) when is_binary(X) -> string;
type(X) when is_integer(X) -> integer;
type(X) when is_float(X) -> float;
type(undefined) -> nil.
