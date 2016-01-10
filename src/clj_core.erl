-module(clj_core).

-export([
         type/1,
         count/1,
         'empty?'/1,
         seq/1,
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
         'string?'/1, 'nil?'/1, 'boolean?'/1, 'regex?'/1,
         deref/1,
         meta/1, with_meta/2, 'meta?'/1,
         get/2, get/3,
         'contains?'/2,
         boolean/1,
         str/1,
         list/1, vector/1, hash_map/1, hash_set/1,
         next_id/0
        ]).

-spec count(any()) -> integer().
count(Seq) ->
  'clojerl.Counted':count(Seq).

-spec 'empty?'(any()) -> integer().
'empty?'(Seq) ->
  'clojerl.Seqable':seq(Seq) == undefined.

-spec seq(any()) -> 'clojerl.List':type().
seq(Seq) ->
  'clojerl.Seqable':seq(Seq).

-spec conj(any(), any()) -> any().
conj(undefined, Item) ->
  list([Item]);
conj(Coll, Item) ->
  'clojerl.IColl':cons(Coll, Item).

%% @doc Clojure's cons builds a cons cell, which is actually
%%      the equivalent to a vanilla Erlang Head and Tail.
%% TODO: it is possible that it should actually return a vanilla
%%       Erlang list.
-spec cons(any(), any()) -> any().
cons(undefined, Item) ->
  list([Item]);
cons(Item, Seq) ->
  'clojerl.IColl':cons(Seq, Item).

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

-spec 'extends?'(atom(), atom()) -> boolean().
'extends?'(Protocol, Type) ->
  ImplModule = 'clojerl.protocol':impl_module(Protocol, Type),
  code:is_loaded(ImplModule) =/= false.

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
  type(X) == 'clojerl.Map'.

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
'nil?'(X) -> type(X) == 'clojerl.nil'.

-spec 'boolean?'(any()) -> boolean().
'boolean?'(X) -> type(X) == 'clojerl.Boolean'.

-spec 'regex?'(any()) -> boolean().
'regex?'(X) -> type(X) == re_pattern.

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

-spec 'contains?'(any(), any()) -> any().
'contains?'(X, Key) -> get(X, Key) =/= undefined.

-spec boolean(any()) -> boolean().
boolean(undefined) -> false;
boolean(false) -> false;
boolean(_) -> true.

-spec type(any()) -> atom().
type(X) when is_tuple(X) -> element(1, X);
type(X) when is_binary(X) -> 'clojerl.String';
type(X) when is_integer(X) -> 'clojerl.Integer';
type(X) when is_float(X) -> 'clojerl.Float';
type(X) when is_boolean(X) -> 'clojerl.Boolean';
type(X) when is_list(X) -> 'clojerl.erlang.List';
type(X) when is_map(X) -> 'clojerl.erlang.Map';
type(undefined) -> 'clojerl.nil';
type(X) when is_atom(X) -> 'clojerl.erlang.Atom';
type(Value) -> throw({Value, <<" has an unsupported type">>}).

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

-spec next_id() -> integer().
next_id() ->
  erlang:unique_integer([positive]).
