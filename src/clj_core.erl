-module(clj_core).

-export([
         type/1,
         count/1,
         first/1,
         next/1,
         rest/1,
         second/1,
         third/1,
         fourth/1,
         name/1,
         namespace/1,
         symbol/1, symbol/2,
         keyword/1, keyword/2,
         'symbol?'/1,
         'keyword?'/1,
         deref/1,
         meta/1,
         get/2, get/3,
         boolean/1,
         str/1
        ]).

-spec count(any()) -> integer().
count(Seq) ->
  'clojerl.Counted':count(Seq).

-spec first(any()) -> any().
first(undefined) -> undefined;
first(Seq) -> 'clojerl.ISeq':first(Seq).

-spec next(any()) -> any().
next(undefined) -> undefined;
next(Seq) -> 'clojerl.ISeq':next(Seq).

-spec rest(any()) -> any().
rest(undefined) -> undefined;
rest(Seq) -> 'clojerl.ISeq':more(Seq).

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

-spec symbol(binary()) -> 'clojerl.Symbol':type().
symbol(Name) ->
  'clojerl.Symbol':new(Name).

-spec symbol(binary(), binary()) -> 'clojerl.Symbol':type().
symbol(Namespace, Name) ->
  'clojerl.Symbol':new(Namespace, Name).

-spec keyword(binary()) -> 'clojerl.Keyword':type().
keyword(Name) ->
  'clojerl.Keyword':new(Name).

-spec keyword(binary(), binary()) -> 'clojerl.Keyword':type().
keyword(Namespace, Name) ->
  'clojerl.Keyword':new(Namespace, Name).

-spec 'symbol?'(any()) -> boolean().
'symbol?'(X) ->
  type(X) == 'clojerl.Symbol'.

-spec 'keyword?'(any()) -> boolean().
'keyword?'(X) ->
  type(X) == 'clojerl.Keyword'.

-spec deref(any()) -> any().
deref(X) ->
  'clojerl.IDeref':deref(X).

-spec meta(any()) -> any().
meta(X) ->
  'clojerl.IMeta':meta(X).

-spec get(any(), any()) -> any().
get(undefined, _Key) -> undefined;
get(X, Key) -> 'clojerl.ILookup':get(X, Key).

-spec get(any(), any(), any()) -> any().
get(undefined, _Key, _NotFound) -> undefined;
get(X, Key, NotFound) -> 'clojerl.ILookup':get(X, Key, NotFound).

-spec boolean(any()) -> boolean().
boolean(undefined) -> false;
boolean(false) -> false;
boolean(_) -> true.

-spec str(any()) -> any().
str(L) when is_list(L) ->
  Strs = lists:map(fun str/1, L),
  binary_join(Strs, <<>>);
str(X) ->
  'clojerl.Stringable':str(X).

-spec type(any()) -> atom().
type(X) when is_tuple(X) -> element(1, X);
type(X) when is_binary(X) -> string;
type(X) when is_integer(X) -> integer;
type(X) when is_float(X) -> float;
type(undefined) -> nil.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
                  case bit_size(B) of
                    0 -> <<A/binary, Sep/binary, B/binary>>;
                    _ -> A
                  end
              end, <<>>, List).
