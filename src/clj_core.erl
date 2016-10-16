-module(clj_core).

-include("clojerl.hrl").

-export([
         type/1,
         load/1, load/2,
         count/1, nth/2, nth/3,
         'empty?'/1, empty/1,
         seq/1, to_list/1,
         equiv/2,
         conj/2, disj/2,
         cons/2,
         first/1, next/1, rest/1,
         second/1, third/1, fourth/1,
         peek/1, pop/1,
         name/1, namespace/1,
         symbol/1, symbol/2,
         keyword/1, keyword/2,
         'satisfies?'/2,
         'coll?'/1, 'sequential?'/1, 'associative?'/1, 'seq?'/1,
         'map?'/1, 'list?'/1, 'vector?'/1, 'set?'/1,
         'symbol?'/1, 'keyword?'/1, 'number?'/1, 'char?'/1,
         'string?'/1, 'nil?'/1, 'boolean?'/1, 'regex?'/1, 'var?'/1,
         deref/1, 'set!'/2,
         meta/1, with_meta/2, 'meta?'/1,
         get/2, get/3,
         assoc/3, dissoc/2, find/2,
         merge/1,
         'contains?'/2,
         boolean/1,
         byte/1, char/1, short/1,
         str/1,
         list/1, vector/1, hash_map/1, hash_set/1,
         subvec/3,
         keys/1, vals/1,
         'even?'/1,
         invoke/2,
         next_id/0,
         gensym/0, gensym/1
        ]).

-spec type(any()) -> atom().
type(X) when is_record(X, ?TYPE) -> X#?TYPE.name;
type(X) when is_binary(X)    -> 'clojerl.String';
type(X) when is_integer(X)   -> 'clojerl.Integer';
type(X) when is_float(X)     -> 'clojerl.Float';
type(X) when is_boolean(X)   -> 'clojerl.Boolean';
type(X) when is_list(X)      -> 'clojerl.erlang.List';
type(X) when is_map(X)       -> 'clojerl.erlang.Map';
type(X) when is_tuple(X)     -> 'clojerl.erlang.Tuple';
type(X) when is_function(X)  -> 'clojerl.erlang.Fn';
type(undefined)              -> 'clojerl.Nil';
type(X) when is_atom(X)      -> 'clojerl.Keyword';
type(X) when is_port(X)      -> 'clojerl.erlang.Port';
type(X) when is_pid(X)       -> 'clojerl.erlang.Process';
type(X) when is_reference(X) -> 'clojerl.erlang.Reference';
type(Value) -> throw({Value, <<" has an unsupported type">>}).

-spec load(binary()) -> undefined.
load(ScriptBase) ->
  load(ScriptBase, true).

-spec load(binary(), boolean()) -> undefined.
load(ScriptBase, FailIfNotFound) ->
  NsBin = binary:replace(ScriptBase, <<"/">>, <<".">>, [global]),
  case load_ns(NsBin) of
    ok -> ok;
    _ ->
      case resolve_file(ScriptBase, [<<".clj">>, <<".cljc">>]) of
        undefined ->
          clj_utils:error_when( FailIfNotFound
                              , [ <<"Could not locate ">>, NsBin
                                , <<".beam or ">>, ScriptBase
                                , <<" on code path.">>
                                ]
                              );
        FullFilePath -> clj_compiler:compile_file(FullFilePath)
      end
  end,
  undefined.

-spec load_ns(binary()) -> ok | error.
load_ns(NsBin) ->
  case code:ensure_loaded(binary_to_atom(NsBin, utf8)) of
    {module, _} -> ok;
    _           -> error
  end.

-spec resolve_file(binary(), [binary()]) -> binary() | undefined.
resolve_file(Path, Exts) ->
  Found =
    [ filename:join(CP, <<Path/binary, Ext/binary>>)
      || CP <- code:get_path(),
         Ext <- Exts,
         filelib:is_regular(filename:join(CP, <<Path/binary, Ext/binary>>))
    ],

  case length(Found) of
    0 -> undefined;
    1 -> first(Found);
    _ -> clj_utils:error([<<"Found more than one ">>, Path])
  end.

-spec count(any()) -> integer().
count(undefined) -> 0;
count(Seq)       -> 'clojerl.Counted':count(Seq).

-spec nth(any(), integer()) -> any().
nth(undefined, _) -> undefined;
nth([], _) -> undefined;
nth(Coll, N) ->
  Type = type(Coll),
  case 'satisfies?'('clojerl.Indexed', Type) of
    true  -> 'clojerl.Indexed':nth(Coll, N);
    false -> nth_from(Coll, N, undefined)
  end.

-spec nth(any(), integer(), any()) -> any().
nth(undefined, _, NotFound) -> NotFound;
nth([], _, NotFound)        -> NotFound;
nth(Coll, N, NotFound) ->
  Type = type(Coll),
  case 'satisfies?'('clojerl.Indexed', Type) of
    true  -> 'clojerl.Indexed':nth(Coll, N, NotFound);
    false -> nth_from(Coll, N, NotFound)
  end.

-spec nth_from(any(), integer(), any()) -> any().
nth_from(Coll, N, NotFound) ->
  Type = type(Coll),
  case 'satisfies?'('clojerl.ISequential', Type) of
    true  -> clj_utils:nth(N + 1, to_list(Coll), NotFound);
    false -> clj_utils:error([<<"Can't apply nth to type ">>, Type])
  end.

-spec 'empty?'(any()) -> integer().
'empty?'(Seq) ->
  'clojerl.Seqable':seq(Seq) == undefined.

-spec empty(any()) -> integer().
empty(Coll) ->
  'clojerl.IColl':empty(Coll).

-spec seq(any()) -> list() | undefined.
seq(Seqable) ->
  'clojerl.Seqable':seq(Seqable).

-spec to_list(any()) -> list().
to_list(undefined) -> [];
to_list(List) when is_list(List) -> List;
to_list(Seqable) ->
  'clojerl.Seqable':to_list(Seqable).

-spec equiv(any(), any()) -> boolean().
equiv(X, Y) ->
  case
    'satisfies?'('clojerl.IEquiv', type(X))
    andalso 'satisfies?'('clojerl.IEquiv', type(Y))
  of
    true  -> 'clojerl.IEquiv':equiv(X, Y);
    false -> X == Y
  end.

-spec conj(any(), any()) -> any().
conj(undefined, Item) ->
  list([Item]);
conj(Coll, Item) ->
  'clojerl.IColl':cons(Coll, Item).

-spec disj(any(), any()) -> any().
disj(undefined, _Item) ->
  undefined;
disj(Coll, Item) ->
  'clojerl.ISet':disjoin(Coll, Item).

%% @doc Clojure's cons builds a cons cell. In most cases it is just
%%      a vanilla Erlang Head and Tail. When dealing with LazySeqs
%%      it is a clojerl.Cons cell, so that the realization of values
%%      can be postponed until they are used.
-spec cons(any(), any()) -> list().
cons(Item, undefined) ->
  list([Item]);
cons(Item, Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.IColl':cons(Seq, Item);
    false -> 'clojerl.IColl':cons(to_list(Seq), Item)
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
rest(undefined) -> [];
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

-spec peek(any()) -> any().
peek(undefined) -> undefined;
peek(Stack)     -> 'clojerl.IStack':peek(Stack).

-spec pop(any()) -> any().
pop(undefined) -> undefined;
pop(Stack)     -> 'clojerl.IStack':pop(Stack).

-spec name(any()) -> any().
name(X) when is_binary(X) -> X;
name(X) -> 'clojerl.Named':name(X).

-spec namespace(any()) -> any().
namespace(X) ->
  'clojerl.Named':namespace(X).

-spec symbol(binary()) -> 'clojerl.Symbol':type().
symbol(Name) ->
  'clojerl.Symbol':?CONSTRUCTOR(Name).

-spec symbol(binary(), binary()) -> 'clojerl.Symbol':type().
symbol(Namespace, Name) ->
  'clojerl.Symbol':?CONSTRUCTOR(Namespace, Name).

-spec keyword(binary()) -> 'clojerl.Keyword':type().
keyword(Name) ->
  'clojerl.Keyword':?CONSTRUCTOR(Name).

-spec keyword(binary(), binary()) -> 'clojerl.Keyword':type().
keyword(Namespace, Name) ->
  'clojerl.Keyword':?CONSTRUCTOR(Namespace, Name).

-spec 'satisfies?'(atom(), atom()) -> boolean().
'satisfies?'(Protocol, Type) ->
  'clojerl.protocol':'satisfies?'(Protocol, Type).

-spec 'coll?'(any()) -> boolean().
'coll?'(X) ->
  'satisfies?'('clojerl.IColl', type(X)).

-spec 'sequential?'(any()) -> boolean().
'sequential?'(X) ->
  'satisfies?'('clojerl.ISequential', type(X)).

-spec 'associative?'(any()) -> boolean().
'associative?'(X) ->
  'satisfies?'('clojerl.Associative', type(X)).

-spec 'seq?'(any()) -> boolean().
'seq?'(X) ->
  'satisfies?'('clojerl.ISeq', type(X)).

-spec 'list?'(any()) -> boolean().
'list?'(X) ->
  type(X) == 'clojerl.List'.

-spec 'vector?'(any()) -> boolean().
'vector?'(X) ->
  type(X) == 'clojerl.Vector'.

-spec 'map?'(any()) -> boolean().
'map?'(X) ->
  'satisfies?'('clojerl.IMap', type(X)).

-spec 'set?'(any()) -> boolean().
'set?'(X) ->
  'satisfies?'('clojerl.ISet', type(X)).

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
'regex?'(X) -> type(X) == 'erlang.util.Regex'.

-spec 'var?'(any()) -> boolean().
'var?'(X) ->
  type(X) == 'clojerl.Var'.

-spec deref(any()) -> any().
deref(X) ->
  'clojerl.IDeref':deref(X).

-spec 'set!'('clojerl.Var':type(), any()) -> any().
'set!'(Var, Value) ->
  'clojerl.Var':dynamic_binding(Var, Value).

-spec meta(any()) -> any().
meta(X) ->
  'clojerl.IMeta':meta(X).

-spec with_meta(any(), any()) -> any().
with_meta(X, Meta) ->
  'clojerl.IMeta':with_meta(X, Meta).

-spec 'meta?'(any()) -> any().
'meta?'(X) ->
  'satisfies?'('clojerl.IMeta', type(X)).

-spec 'contains?'(any(), any()) -> boolean().
'contains?'(undefined, _) ->
  false;
'contains?'(Coll, Key) ->
  IsAssociative = 'associative?'(Coll),
  IsSet = 'set?'(Coll),

  if
    IsAssociative -> 'clojerl.Associative':contains_key(Coll, Key);
    IsSet -> 'clojerl.ISet':contains(Coll, Key);
    true  ->
      clj_utils:error([ "contains? not supported on type: ", name(type(Coll))])
  end.

-spec get(any(), any()) -> any().
get(undefined, _Key) -> undefined;
get(X, Key) ->
  case 'set?'(X) of
    true  -> 'clojerl.ISet':get(X, Key);
    false -> 'clojerl.ILookup':get(X, Key)
  end.

-spec get(any(), any(), any()) -> any().
get(undefined, _Key, NotFound) -> NotFound;
get(X, Key, NotFound) ->
  case 'set?'(X) of
    true  ->
      case 'clojerl.ISet':'contains'(X, Key) of
        true  -> 'clojerl.ISet':get(X, Key);
        false -> NotFound
      end;
    false -> 'clojerl.ILookup':get(X, Key, NotFound)
  end.

-spec assoc('clojerl.Associative':type(), any(), any()) ->
  'clojerl.Associative':type().
assoc(undefined, Key, Value) ->
  hash_map([Key, Value]);
assoc(Map, Key, Value) ->
  'clojerl.Associative':assoc(Map, Key, Value).

-spec dissoc('clojerl.IMap':type(), any()) -> 'clojerl.IMap':type().
dissoc(undefined, _Key) ->
  undefined;
dissoc(Map, Key) ->
  'clojerl.IMap':without(Map, Key).

-spec find(any(), any()) -> any().
find(undefined, _) ->
  undefined;
find(Map, Key) ->
  case 'associative?'(Map) of
    true  -> 'clojerl.Associative':entry_at(Map, Key);
    false -> undefined
  end.

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
  Result = lists:foldl(ConjFun, First, to_list(Second)),
  merge([Result | Rest]).

-spec boolean(any()) -> boolean().
boolean(undefined) -> false;
boolean(false) -> false;
boolean(_) -> true.

-spec byte(any()) -> boolean().
byte(X) when is_number(X), 0 =< X, X =< 256 ->
  erlang:trunc(X).

-spec char(any()) -> boolean().
char(X) when is_number(X) ->
  case unicode:characters_to_binary([X], utf8) of
    Char when is_binary(Char) -> Char;
    Error -> error(Error)
  end.

-spec short(any()) -> boolean().
short(X) when is_number(X), 0 =< X, X < 32768 ->
  erlang:trunc(X).

-spec str(any()) -> binary().
str(X) ->
  'clojerl.Stringable':str(X).

-spec 'list'(list()) -> 'clojerl.List':type().
list(Items) ->
  'clojerl.List':?CONSTRUCTOR(Items).

-spec vector(list()) -> 'clojerl.Vector':type().
vector(Items) when is_list(Items) ->
  'clojerl.Vector':?CONSTRUCTOR(Items);
vector(Items) ->
  vector(to_list(Items)).

-spec subvec('clojerl.Vector':type(), integer(), integer()) ->
  'clojerl.Vector':type().
subvec(Vector, Start, End) ->
  clj_utils:error_when(End < Start
                       orelse Start < 0
                       orelse End > count(Vector),
                       ["Index out of bounds"]
                      ),
  case Start of
    End -> vector([]);
    _   -> 'clojerl.Vector':subvec(Vector, Start, End)
  end.

-spec hash_map(list()) -> 'clojerl.Map':type().
hash_map(Items) ->
  case count(Items) of
    0 -> 'clojerl.Map':?CONSTRUCTOR([]);
    _ -> 'clojerl.Map':?CONSTRUCTOR(seq(Items))
  end.

-spec hash_set(list()) -> 'clojerl.Set':type().
hash_set(Items) ->
  case count(Items) of
    0 -> 'clojerl.Set':?CONSTRUCTOR([]);
    _ -> 'clojerl.Set':?CONSTRUCTOR(seq(Items))
  end.

-spec keys('clojerl.IMap':type()) -> list().
keys(Map) ->
  'clojerl.IMap':keys(Map).

-spec vals('clojerl.IMap':type()) -> list().
vals(undefined) -> undefined;
vals(Map) -> 'clojerl.IMap':vals(Map).

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
