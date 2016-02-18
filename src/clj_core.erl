-module(clj_core).

-include("clojerl.hrl").

-export([
         type/1,
         count/1,
         'empty?'/1, empty/1,
         seq/1, seq2/1,
         equiv/2,
         conj/2,
         cons/2,
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
         'extends?'/2,
         'coll?'/1,
         'sequential?'/1,
         'seq?'/1,
         'map?'/1, 'list?'/1, 'vector?'/1, 'set?'/1,
         'symbol?'/1, 'keyword?'/1, 'number?'/1, 'char?'/1,
         'string?'/1, 'nil?'/1, 'boolean?'/1, 'regex?'/1, 'var?'/1,
         deref/1,
         meta/1, with_meta/2, 'meta?'/1,
         get/2, get/3,
         assoc/3,
         merge/1,
         'contains?'/2,
         boolean/1,
         str/1,
         list/1, vector/1, hash_map/1, hash_set/1,
         keys/1, vals/1,
         'even?'/1,
         invoke/2,
         next_id/0,
         gensym/0, gensym/1
        ]).

-spec type(any()) -> atom().
type(X) when is_record(X, ?TYPE) -> X#?TYPE.name;
type(X) when is_binary(X)   -> 'clojerl.String';
type(X) when is_integer(X)  -> 'clojerl.Integer';
type(X) when is_float(X)    -> 'clojerl.Float';
type(X) when is_boolean(X)  -> 'clojerl.Boolean';
type(X) when is_list(X)     -> 'clojerl.erlang.List';
type(X) when is_map(X)      -> 'clojerl.erlang.Map';
type(X) when is_tuple(X)    -> 'clojerl.erlang.Tuple';
type(X) when is_function(X) -> 'clojerl.erlang.Fn';
type(undefined)             -> 'clojerl.Nil';
type(X) when is_atom(X)     -> 'clojerl.Keyword';
type(Value) -> throw({Value, <<" has an unsupported type">>}).

-spec count(any()) -> integer().
count(Seq) ->
  'clojerl.Counted':count(Seq).

-spec 'empty?'(any()) -> integer().
'empty?'(Seq) ->
  'clojerl.Seqable':seq(Seq) == undefined.

-spec empty(any()) -> integer().
empty(Coll) ->
  'clojerl.IColl':empty(Coll).

-spec seq(any()) -> list() | undefined.
seq(Seqable) ->
  'clojerl.Seqable':seq(Seqable).

-spec seq2(any()) -> list().
seq2(Seqable) ->
  case seq(Seqable) of
    undefined -> [];
    Seq -> Seq
  end.

-spec equiv(any(), any()) -> boolean().
equiv(X, Y) ->
  case
    'extends?'('clojerl.IEquiv', type(X))
    andalso 'extends?'('clojerl.IEquiv', type(Y))
  of
    true  -> 'clojerl.IEquiv':equiv(X, Y);
    false -> X == Y
  end.

-spec conj(any(), any()) -> any().
conj(undefined, Item) ->
  list([Item]);
conj(Coll, Item) ->
  'clojerl.IColl':cons(Coll, Item).

%% @doc Clojure's cons builds a cons cell, which is actually
%%      the equivalent to a vanilla Erlang Head and Tail.
%% TODO: it is possible that it should actually return a vanilla
%%       Erlang list.
-spec cons(any(), any()) -> list().
cons(Item, undefined) ->
  list([Item]);
cons(Item, Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.IColl':cons(Seq, Item);
    false -> 'clojerl.IColl':cons(seq2(Seq), Item)
  end.

-spec first(any()) -> any().
first(undefined) -> undefined;
first(Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.ISeq':first(Seq);
    false -> first(seq(Seq))
  end.

-spec next(any()) -> any().
next(undefined) -> undefined;
next(Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.ISeq':next(Seq);
    false -> next(seq(Seq))
  end.

-spec rest(any()) -> any().
rest(undefined) -> undefined;
rest(Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.ISeq':more(Seq);
    false -> rest(seq(Seq))
  end.

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

-spec 'extends?'(atom(), atom()) -> boolean().
'extends?'(Protocol, Type) ->
  'clojerl.protocol':'extends?'(Protocol, Type).

-spec 'coll?'(any()) -> boolean().
'coll?'(X) ->
  'extends?'('clojerl.IColl', type(X)).

-spec 'sequential?'(any()) -> boolean().
'sequential?'(X) ->
  'extends?'('clojerl.ISequential', type(X)).

-spec 'seq?'(any()) -> boolean().
'seq?'(X) ->
  'extends?'('clojerl.ISeq', type(X)).

-spec 'list?'(any()) -> boolean().
'list?'(X) ->
  type(X) == 'clojerl.List'.

-spec 'vector?'(any()) -> boolean().
'vector?'(X) ->
  type(X) == 'clojerl.Vector'.

-spec 'map?'(any()) -> boolean().
'map?'(X) ->
  'extends?'('clojerl.IMap', type(X)).

-spec 'set?'(any()) -> boolean().
'set?'(X) ->
  type(X) == 'clojerl.Set'.

-spec 'symbol?'(any()) -> boolean().
'symbol?'(X) ->
  type(X) == 'clojerl.Symbol'.

-spec 'keyword?'(any()) -> boolean().
'keyword?'(X) ->
  type(X) == 'clojerl.Keyword'.

-spec 'number?'(any()) -> boolean().
'number?'(X) -> type(X) == 'clojerl.Integer' orelse type(X) == 'clojerl.Float'.

-spec 'char?'(any()) -> boolean().
'char?'(X) -> type(X) == 'clojerl.Integer'.

-spec 'string?'(any()) -> boolean().
'string?'(X) -> type(X) == 'clojerl.String'.

-spec 'nil?'(any()) -> boolean().
'nil?'(X) -> type(X) == 'clojerl.Nil'.

-spec 'boolean?'(any()) -> boolean().
'boolean?'(X) -> type(X) == 'clojerl.Boolean'.

-spec 'regex?'(any()) -> boolean().
'regex?'(X) -> type(X) == re_pattern.

-spec 'var?'(any()) -> boolean().
'var?'(X) ->
  type(X) == 'clojerl.Var'.

-spec deref(any()) -> any().
deref(X) ->
  'clojerl.IDeref':deref(X).

-spec meta(any()) -> any().
meta(X) ->
  'clojerl.IMeta':meta(X).

-spec with_meta(any(), any()) -> any().
with_meta(X, Meta) ->
  'clojerl.IMeta':with_meta(X, Meta).

-spec 'meta?'(any()) -> any().
'meta?'(X) ->
  'extends?'('clojerl.IMeta', type(X)).

-spec get(any(), any()) -> any().
get(undefined, _Key) -> undefined;
get(X, Key) -> 'clojerl.ILookup':get(X, Key).

-spec get(any(), any(), any()) -> any().
get(undefined, _Key, _NotFound) -> undefined;
get(X, Key, NotFound) -> 'clojerl.ILookup':get(X, Key, NotFound).

-spec assoc('clojerl.Associative':type(), any(), any()) -> 
  'clojerl.Associative':type().
assoc(undefined, Key, Value) ->
  hash_map([Key, Value]);
assoc(Map, Key, Value) ->
  'clojerl.Associative':assoc(Map, Key, Value).

-spec merge([any()]) -> any().
merge([]) ->
  undefined;
merge([Map]) ->
  Map;
merge([undefined | Maps]) ->
  merge(Maps);
merge([First, undefined | Rest]) ->
  merge([First | Rest]);
merge([First, Second | Rest]) ->
  ConjFun = fun(Item, Acc) -> conj(Acc, Item) end,
  Result = lists:foldl(ConjFun, First, seq2(Second)),
  merge([Result | Rest]).

-spec 'contains?'(any(), any()) -> any().
'contains?'(X, Key) -> get(X, Key) =/= undefined.

-spec boolean(any()) -> boolean().
boolean(undefined) -> false;
boolean(false) -> false;
boolean(_) -> true.

-spec str(any()) -> any().
str(X) ->
  'clojerl.Stringable':str(X).

-spec 'list'(list()) -> 'clojerl.List':type().
list(Items) ->
  'clojerl.List':new(Items).

-spec vector(list()) -> 'clojerl.Vector':type().
vector(Items) ->
  'clojerl.Vector':new(Items).

-spec hash_map(list()) -> 'clojerl.Map':type().
hash_map(Items) ->
  'clojerl.Map':new(Items).

-spec hash_set(list()) -> 'clojerl.Set':type().
hash_set(Items) ->
  'clojerl.Set':new(Items).

-spec keys('clojerl.IMap':type()) -> list().
keys(Map) ->
  'clojerl.IMap':keys(Map).

-spec vals('clojerl.IMap':type()) -> list().
vals(Map) ->
  'clojerl.IMap':vals(Map).

-spec 'even?'(integer()) -> boolean().
'even?'(X) ->
  (X band 1) == 0.

-spec invoke('clojerl.IFn':type(), 'clojerl.ISequential':type()) -> any().
invoke(Fn, Args) ->
  'clojerl.IFn':invoke(Fn, Args).

-spec next_id() -> integer().
next_id() ->
  erlang:unique_integer([positive]).

-spec gensym() -> 'clojer.Symbol':type().
gensym() ->
  gensym(<<"G__">>).

-spec gensym(binary()) -> 'clojer.Symbol':type().
gensym(Prefix) ->
  PartsBin = lists:map(fun str/1, [Prefix, next_id()]),
  symbol(iolist_to_binary(PartsBin)).
