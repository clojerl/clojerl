-module(clj_rt).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ type/1, type_module/1
        , load/1, load/2
        , count/1, nth/2, nth/3
        , 'empty?'/1, empty/1
        , seq/1, seq_or_else/1, to_list/1
        , equiv/2
        , conj/2, disj/2
        , cons/2
        , first/1, next/1, rest/1
        , second/1, third/1, fourth/1
        , peek/1, pop/1
        , name/1, namespace/1
        , symbol/1, symbol/2
        , keyword/1, keyword/2
        , 'coll?'/1, 'sequential?'/1, 'associative?'/1, 'seq?'/1
        , 'map?'/1, 'list?'/1, 'vector?'/1, 'set?'/1
        , 'record?'/1, 'type?'/1
        , 'symbol?'/1, 'keyword?'/1, 'number?'/1, 'char?'/1
        , 'string?'/1, 'nil?'/1, 'boolean?'/1, 'regex?'/1, 'var?'/1
        , deref/1, 'set!'/2
        , meta/1, with_meta/2, 'meta?'/1
        , get/2, get/3
        , assoc/3, dissoc/2, find/2
        , merge/1
        , 'contains?'/2
        , boolean/1
        , byte/1, char/1, short/1
        , str/1, print/1
        , list/1, vector/1, hash_map/1, hash_set/1
        , subvec/3
        , keys/1, vals/1
        , 'even?'/1
        , apply/2
        , next_id/0
        , gensym/0, gensym/1
        , compare_fun/2
        , shuffle/1
        , hash/1
        , '->erl'/2
        ]).

-spec type(any()) -> 'erlang.Type':type().
type(X) ->
  'erlang.Type':?CONSTRUCTOR(type_module(X)).

-spec type_module(any()) -> module().
type_module(#{?TYPE := Name})       -> Name;
type_module(X) when is_binary(X)    -> 'clojerl.String';
type_module(X) when is_bitstring(X) -> 'clojerl.BitString';
type_module(X) when is_integer(X)   -> 'clojerl.Integer';
type_module(X) when is_float(X)     -> 'clojerl.Float';
type_module(X) when is_boolean(X)   -> 'clojerl.Boolean';
type_module(X) when is_list(X)      -> 'erlang.List';
type_module(X) when is_map(X)       -> 'erlang.Map';
type_module(X) when is_tuple(X)     -> 'erlang.Tuple';
type_module(X) when is_function(X)  -> 'erlang.Fn';
type_module(?NIL)                   -> ?NIL_TYPE;
type_module(X) when is_atom(X)      -> 'clojerl.Keyword';
type_module(X) when is_port(X)      -> 'erlang.Port';
type_module(X) when is_pid(X)       -> 'erlang.Process';
type_module(X) when is_reference(X) -> 'erlang.Reference';
type_module(Value) -> throw({Value, <<" has an unsupported type">>}).

-spec load(binary()) -> ?NIL.
load(ScriptBase) ->
  load(ScriptBase, true).

-spec load(binary(), boolean()) -> ?NIL.
load(ScriptBase, FailIfNotFound) ->
  NsBin = clj_utils:resource_to_ns(ScriptBase),
  case load_ns(NsBin) of
    ok -> ok;
    _ ->
      case resolve_file(ScriptBase, [<<".clje">>, <<".cljc">>]) of
        ?NIL ->
          ?ERROR_WHEN( FailIfNotFound
                     , [ <<"Could not locate ">>, NsBin
                       , <<".beam or ">>, ScriptBase
                       , <<" on code path.">>
                       ]
                     );
        FullFilePath -> clj_compiler:compile_file(FullFilePath)
      end
  end,
  ?NIL.

-spec load_ns(binary()) -> ok | error.
load_ns(NsBin) ->
  case code:ensure_loaded(binary_to_atom(NsBin, utf8)) of
    {module, _} -> ok;
    _           -> error
  end.

-spec resolve_file(binary(), [binary()]) -> binary() | ?NIL.
resolve_file(Path, Exts) ->
  Found =
    [ filename:join(CP, <<Path/binary, Ext/binary>>)
      || CP <- code:get_path(),
         Ext <- Exts,
         filelib:is_regular(filename:join(CP, <<Path/binary, Ext/binary>>))
    ],

  case length(Found) of
    0 -> ?NIL;
    1 -> first(Found);
    _ -> ?ERROR([<<"Found more than one ">>, Path])
  end.

-spec count(any()) -> integer().
count(?NIL) -> 0;
count(Seq)       -> 'clojerl.ICounted':count(Seq).

-spec nth(any(), integer()) -> any().
nth(?NIL, _) -> ?NIL;
nth([], _) -> ?NIL;
nth(Coll, N) ->
  Type = type_module(Coll),
  case 'clojerl.IIndexed':?SATISFIES(Type) of
    true  -> 'clojerl.IIndexed':nth(Coll, N);
    false -> nth_from(Coll, N)
  end.

-spec nth(any(), integer(), any()) -> any().
nth(?NIL, _, NotFound) -> NotFound;
nth([], _, NotFound)        -> NotFound;
nth(Coll, N, NotFound) ->
  Type = type_module(Coll),
  case 'clojerl.IIndexed':?SATISFIES(Type) of
    true  -> 'clojerl.IIndexed':nth(Coll, N, NotFound);
    false -> nth_from(Coll, N, NotFound)
  end.

-spec nth_from(any(), integer()) -> any().
nth_from(Coll, N) ->
  Type = type_module(Coll),
  case Type of
    'clojerl.String' ->
      case N < 'clojerl.String':count(Coll) of
        true  -> 'clojerl.String':char_at(Coll, N);
        false -> ?ERROR(<<"Index out of bounds">>)
      end;
    _ ->
      case 'clojerl.ISequential':?SATISFIES(Type) of
        true  -> clj_utils:nth(N + 1, to_list(Coll));
        false -> ?ERROR([<<"Can't apply nth to type ">>, Type])
      end
  end.

-spec nth_from(any(), integer(), any()) -> any().
nth_from(Coll, N, NotFound) ->
  Type = type_module(Coll),
  case Type of
    'clojerl.String' ->
      case N < 'clojerl.String':count(Coll) of
        true  -> 'clojerl.String':char_at(Coll, N);
        false -> NotFound
      end;
    _ ->
      case 'clojerl.ISequential':?SATISFIES(Type) of
        true  -> clj_utils:nth(N + 1, to_list(Coll), NotFound);
        false -> ?ERROR([<<"Can't apply nth to type ">>, Type])
      end
  end.

-spec 'empty?'(any()) -> boolean().
'empty?'(?NIL) -> true;
'empty?'(Seq)  -> 'clojerl.ISeqable':seq(Seq) == ?NIL.

-spec empty(any()) -> any().
empty(Coll) ->
  'clojerl.IColl':empty(Coll).

-spec seq(any()) -> any() | ?NIL.
seq(?NIL)    -> ?NIL;
seq(Seqable) -> 'clojerl.ISeqable':seq(Seqable).

-spec seq_or_else(any()) -> any() | ?NIL.
seq_or_else(Seqable) ->
  case seq(Seqable) of
    ?NIL -> ?NIL;
    _    -> Seqable
  end.

-spec to_list(any()) -> [any()].
to_list(?NIL) -> [];
to_list(List) when is_list(List) -> List;
to_list(Seqable) ->
  'clojerl.ISeqable':to_list(Seqable).

-spec equiv(any(), any()) -> boolean().
equiv(X, Y) ->
  case
    'clojerl.IEquiv':?SATISFIES(type_module(X))
    andalso 'clojerl.IEquiv':?SATISFIES(type_module(Y))
  of
    true  -> 'clojerl.IEquiv':equiv(X, Y);
    false -> X == Y
  end.

-spec conj(any(), any()) -> any().
conj(?NIL, Item) ->
  list([Item]);
conj(Coll, Item) ->
  'clojerl.IColl':cons(Coll, Item).

-spec disj(any(), any()) -> any().
disj(?NIL, _Item) ->
  ?NIL;
disj(Coll, Item) ->
  'clojerl.ISet':disjoin(Coll, Item).

%% @doc Clojure's cons builds a cons cell. In most cases it is just
%%      a vanilla Erlang Head and Tail. When dealing with LazySeqs
%%      it is a clojerl.Cons cell, so that the realization of values
%%      can be postponed until they are used.
-spec cons(any(), any()) -> 'clojerl.Cons':type() | 'clojerl.List':type().
cons(Item, ?NIL) ->
  list([Item]);
cons(Item, Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.Cons':?CONSTRUCTOR(Item, Seq);
    false -> 'clojerl.Cons':?CONSTRUCTOR(Item, seq(Seq))
  end.

-spec first(any()) -> any().
first(?NIL) -> ?NIL;
first(Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.ISeq':first(Seq);
    false -> first(seq(Seq))
  end.

-spec next(any()) -> any().
next(?NIL) -> ?NIL;
next(Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.ISeq':next(Seq);
    false -> next(seq(Seq))
  end.

-spec rest(any()) -> any().
rest(?NIL) -> [];
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
peek(?NIL) -> ?NIL;
peek(Stack)     -> 'clojerl.IStack':peek(Stack).

-spec pop(any()) -> any().
pop(?NIL) -> ?NIL;
pop(Stack)     -> 'clojerl.IStack':pop(Stack).

-spec name(any()) -> binary() | ?NIL.
name(X) when is_binary(X) -> X;
name(X) -> 'clojerl.INamed':name(X).

-spec namespace(any()) -> binary() | ?NIL.
namespace(X) ->
  'clojerl.INamed':namespace(X).

-spec symbol(binary()) -> 'clojerl.Symbol':type().
symbol(Name) ->
  'clojerl.Symbol':?CONSTRUCTOR(Name).

-spec symbol(binary(), binary()) -> 'clojerl.Symbol':type().
symbol(Namespace, Name) ->
  'clojerl.Symbol':?CONSTRUCTOR(Namespace, Name).

-spec keyword('clojerl.Symbol':type() | binary()) -> 'clojerl.Keyword':type().
keyword(Name) ->
  'clojerl.Keyword':?CONSTRUCTOR(Name).

-spec keyword(binary(), binary()) -> 'clojerl.Keyword':type().
keyword(Namespace, Name) ->
  'clojerl.Keyword':?CONSTRUCTOR(Namespace, Name).

-spec 'coll?'(any()) -> boolean().
'coll?'(X) ->
  'clojerl.IColl':?SATISFIES(type_module(X)).

-spec 'sequential?'(any()) -> boolean().
'sequential?'(X) ->
  'clojerl.ISequential':?SATISFIES(type_module(X)).

-spec 'associative?'(any()) -> boolean().
'associative?'(X) ->
  'clojerl.IAssociative':?SATISFIES(type_module(X)).

-spec 'seq?'(any()) -> boolean().
'seq?'(X) ->
  'clojerl.ISeq':?SATISFIES(type_module(X)).

-spec 'list?'(any()) -> boolean().
'list?'(X) ->
  type_module(X) == 'clojerl.List'.

-spec 'vector?'(any()) -> boolean().
'vector?'(X) ->
  type_module(X) == 'clojerl.Vector'.

-spec 'map?'(any()) -> boolean().
'map?'(X) ->
  'clojerl.IMap':?SATISFIES(type_module(X)).

-spec 'set?'(any()) -> boolean().
'set?'(X) ->
  'clojerl.ISet':?SATISFIES(type_module(X)).

-spec 'record?'(any()) -> boolean().
'record?'(X) ->
  'clojerl.IRecord':?SATISFIES(type_module(X)).

-spec 'type?'(any()) -> boolean().
'type?'(X) ->
  'clojerl.IType':?SATISFIES(type_module(X)).

-spec 'symbol?'(any()) -> boolean().
'symbol?'(X) ->
  type_module(X) == 'clojerl.Symbol'.

-spec 'keyword?'(any()) -> boolean().
'keyword?'(X) ->
  type_module(X) == 'clojerl.Keyword'.

-spec 'number?'(any()) -> boolean().
'number?'(X) ->
  type_module(X) == 'clojerl.Integer'
    orelse type_module(X) == 'clojerl.Float'.

-spec 'char?'(any()) -> boolean().
'char?'(<<_/utf8>>) -> true;
'char?'(_)          -> false.

-spec 'string?'(any()) -> boolean().
'string?'(X) -> type_module(X) == 'clojerl.String'.

-spec 'nil?'(any()) -> boolean().
'nil?'(X) -> type_module(X) == ?NIL_TYPE.

-spec 'boolean?'(any()) -> boolean().
'boolean?'(X) -> type_module(X) == 'clojerl.Boolean'.

-spec 'regex?'(any()) -> boolean().
'regex?'(X) -> type_module(X) == 'erlang.util.Regex'.

-spec 'var?'(any()) -> boolean().
'var?'(X) ->
  type_module(X) == 'clojerl.Var'.

-spec deref(any()) -> any().
deref(X) ->
  'clojerl.IDeref':deref(X).

-spec 'set!'('clojerl.Var':type(), any()) -> any().
'set!'(Var, Value) ->
  'clojerl.Var':dynamic_binding(Var, Value).

-spec meta(any()) -> any().
meta(X) ->
  'clojerl.IMeta':meta(X).

-spec with_meta(T, 'clojerl.Map':type()) -> T.
with_meta(X, Meta) ->
  'clojerl.IMeta':with_meta(X, Meta).

-spec 'meta?'(any()) -> any().
'meta?'(X) ->
  'clojerl.IMeta':?SATISFIES(type_module(X)).

-spec 'contains?'(any(), any()) -> boolean().
'contains?'(?NIL, _) ->
  false;
'contains?'(Coll, Key) ->
  IsAssociative = 'associative?'(Coll),
  IsSet = 'set?'(Coll),

  if
    IsAssociative -> 'clojerl.IAssociative':contains_key(Coll, Key);
    IsSet -> 'clojerl.ISet':contains(Coll, Key);
    true  -> ?ERROR(["contains? not supported on type: ", type(Coll)])
  end.

-spec get(any(), any()) -> any().
get(?NIL, _Key) -> ?NIL;
get(X, Key) ->
  case 'clojerl.ILookup':?SATISFIES(type_module(X)) of
    true  -> 'clojerl.ILookup':get(X, Key);
    false -> ?NIL
  end.

-spec get(any(), any(), any()) -> any().
get(?NIL, _Key, NotFound) -> NotFound;
get(X, Key, NotFound) ->
  case 'clojerl.ILookup':?SATISFIES(type_module(X)) of
    true  -> 'clojerl.ILookup':get(X, Key, NotFound);
    false -> NotFound
  end.

-spec assoc('clojerl.IAssociative':type(), any(), any()) ->
  'clojerl.IAssociative':type().
assoc(?NIL, Key, Value) ->
  hash_map([Key, Value]);
assoc(Map, Key, Value) ->
  'clojerl.IAssociative':assoc(Map, Key, Value).

-spec dissoc('clojerl.IMap':type(), any()) -> 'clojerl.IMap':type().
dissoc(?NIL, _Key) ->
  ?NIL;
dissoc(Map, Key) ->
  'clojerl.IMap':without(Map, Key).

-spec find(any(), any()) -> any().
find(?NIL, _) ->
  ?NIL;
find(Map, Key) ->
  case 'associative?'(Map) of
    true  -> 'clojerl.IAssociative':entry_at(Map, Key);
    false -> ?NIL
  end.

-spec merge([any()]) -> any().
merge([]) ->
  ?NIL;
merge([Map]) ->
  Map;
merge([?NIL | Maps]) ->
  merge(Maps);
merge([First, ?NIL | Rest]) ->
  merge([First | Rest]);
merge([First, Second | Rest]) ->
  ConjFun = fun(Item, Acc) -> conj(Acc, Item) end,
  Result = lists:foldl(ConjFun, First, to_list(Second)),
  merge([Result | Rest]).

-spec boolean(any()) -> boolean().
boolean(?NIL) -> false;
boolean(false) -> false;
boolean(_) -> true.

-spec byte(number()) -> integer().
byte(X) when is_number(X), 0 =< X, X < 256 ->
  erlang:trunc(X).

-spec char(integer()) -> binary().
char(X) when is_number(X) ->
  case unicode:characters_to_binary([erlang:trunc(X)], utf8) of
    Char when is_binary(Char) -> Char;
    Error -> error(Error)
  end.

-spec short(number()) -> integer().
short(X) when is_number(X), 0 =< X, X < 32768 ->
  erlang:trunc(X).

-spec str(any()) -> binary().
str(?NIL) -> <<"">>;
str(X)    -> 'clojerl.IStringable':str(X).

-spec print(any()) -> binary().
print(X) ->
  case 'clojure.core':'print-initialized__val'() of
    true  ->
      StringWriter = 'erlang.io.StringWriter':?CONSTRUCTOR(),
      try
        'clojure.core':'pr-on'(X, StringWriter),
        'erlang.io.StringWriter':str(StringWriter)
      after
        'erlang.io.StringWriter':close(StringWriter)
      end;
    false ->
      PrintReadably = boolean('clojure.core':'*print-readably*__val'()),
      Type          = type_module(X),
      do_print(X, Type, PrintReadably)
  end.

-spec do_print(any(), atom(), boolean()) -> binary().
do_print(?NIL, _, _) ->
  <<"nil">>;
do_print(X, 'clojerl.String', true) ->
  do_print_string(X, <<"\"">>);
do_print(X, 'clojerl.String', false) ->
  X;
do_print(X, 'clojerl.Cons', _PrintReadably) ->
  do_print_seq(X, <<"(">>, <<")">>);
do_print(X, 'clojerl.LazySeq', _PrintReadably) ->
  do_print_seq(X, <<"(">>, <<")">>);
do_print(X, 'clojerl.List', _PrintReadably) ->
  do_print_seq(X, <<"(">>, <<")">>);
do_print(X, 'clojerl.Map', _PrintReadably) ->
  do_print_map(X);
do_print(X, 'clojerl.Range', _PrintReadably) ->
  do_print_seq(X, <<"(">>, <<")">>);
do_print(X, 'clojerl.Set', _PrintReadably) ->
  do_print_seq(X, <<"#{">>, <<"}">>);
do_print(X, 'clojerl.SortedSet', _PrintReadably) ->
  do_print_seq(X, <<"#{">>, <<"}">>);
do_print(X, 'clojerl.SortedMap', _PrintReadably) ->
  do_print_map(X);
do_print(X, 'clojerl.TupleMap', _PrintReadably) ->
  do_print_map(X);
do_print(X, 'clojerl.Vector', _PrintReadably) ->
  do_print_seq(X, <<"[">>, <<"]">>);
do_print(X, 'erlang.List', _PrintReadably) ->
  do_print_seq(X, <<"#erl(">>, <<")">>);
do_print(X, 'erlang.Map', _PrintReadably) ->
  do_print_map(X, <<"#erl{">>, <<"}">>);
do_print(X, 'erlang.Tuple', _PrintReadably) ->
  do_print_seq(X, <<"#erl[">>, <<"]">>);
do_print(X, _Type, _PrintReadably) ->
  str(X).

-spec do_print_string(binary(), binary()) -> binary().
do_print_string(<<>>, Acc) ->
  <<Acc/binary, "\"">>;
do_print_string(<<X/utf8, Rest/binary>>, Acc) ->
  Ch = case <<X>> of
         <<"\n">> -> <<"\\n">>;
         <<"\t">> -> <<"\\t">>;
         <<"\r">> -> <<"\\r">>;
         <<"\"">> -> <<"\\\"">>;
         <<"\\">> -> <<"\\\\">>;
         <<"\f">> -> <<"\\f">>;
         <<"\b">> -> <<"\\b">>;
         Default  -> Default
       end,
  do_print_string(Rest, <<Acc/binary, Ch/binary>>).

-spec do_print_seq(any(), binary(), binary()) -> binary().
do_print_seq(X, First, Last) ->
  Items = lists:map(fun print/1, to_list(X)),
  Strs  = 'clojerl.String':join(Items, <<" ">>),
  <<First/binary, Strs/binary, Last/binary>>.

-spec do_print_map(any()) -> binary().
do_print_map(X) ->
  do_print_map(X, <<"{">>, <<"}">>).

-spec do_print_map(any(), binary(), binary()) -> binary().
do_print_map(X, First, Last) ->
  FunItem  = fun(Item) ->
                 [K, V] = to_list(Item),
                 Key    = print(K),
                 Value  = print(V),
                 <<Key/binary, " ", Value/binary>>
             end,
  ItemsStr = lists:map(FunItem, to_list(X)),
  Strs  = 'clojerl.String':join(ItemsStr, <<", ">>),
  <<First/binary, Strs/binary, Last/binary>>.

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
  ?ERROR_WHEN(End < Start
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
vals(?NIL) -> ?NIL;
vals(Map) -> 'clojerl.IMap':vals(Map).

-spec 'even?'(integer()) -> boolean().
'even?'(X) ->
  (X band 1) == 0.

-spec apply('clojerl.IFn':type(), 'clojerl.ISequential':type()) -> any().
apply(Fn, Args) ->
  'clojerl.IFn':apply(Fn, Args).

-spec next_id() -> integer().
next_id() ->
  N = case erlang:get(gensym_count) of
        undefined -> 0;
        X -> X
      end,
  erlang:put(gensym_count, N + 1),
  N.

-spec gensym() -> 'clojer.Symbol':type().
gensym() ->
  gensym(<<"G__">>).

-spec gensym(binary()) -> 'clojer.Symbol':type().
gensym(Prefix) ->
  PartsBin = [Prefix, integer_to_list(next_id())],
  symbol(iolist_to_binary(PartsBin)).

-spec compare_fun('erlang.Fn':type(), erlang | clojure) -> function().
compare_fun(Fun, erlang) ->
  fun(X, Y) -> clj_rt:apply(Fun, [X, Y]) =< 0 end;
compare_fun(Fun, clojure) ->
  fun(X, Y) -> clj_rt:apply(Fun, [X, Y]) end.

-spec shuffle(any()) -> [any()].
shuffle(Seq) ->
  Items = [{rand:uniform(), X} || X <- to_list(Seq)],
  [X || {_, X} <- lists:sort(Items)].

-spec hash(any()) -> integer().
hash(?NIL) -> 0;
hash(X)    -> 'clojerl.IHash':hash(X).

-spec '->erl'(any(), boolean()) -> any().
'->erl'(?NIL, _)      -> ?NIL;
'->erl'(X, Recursive) ->
  case 'clojerl.IErl':?SATISFIES(type_module(X)) of
    true  -> 'clojerl.IErl':'->erl'(X, Recursive);
    false -> X
  end.
