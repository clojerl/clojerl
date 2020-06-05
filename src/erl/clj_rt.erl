-module(clj_rt).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([ type/1, type_module/1
        , load/1, load/2
        , load_script/2
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
        , str/1, print_str/1, print/2
        , list/1, vector/1, hash_map/1, hash_set/1
        , subvec/3
        , keys/1, vals/1
        , 'even?'/1
        , apply/2
        , reset_id/0, next_id/0
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
        FullFilePath -> clj_compiler:file(FullFilePath)
      end
  end,
  ?NIL.

-spec load_script(binary(), boolean()) -> any().
load_script(ScriptName, FailIfNotFound) ->
  File = filename:basename(ScriptName),
  Ext  = filename:extension(File),
  Name = filename:basename(File, Ext),
  case resolve_file(Name, [Ext]) of
    ?NIL ->
      ?ERROR_WHEN( FailIfNotFound
                 , [ <<"Could not locate Clojure resource on code path: ">>
                   , ScriptName
                   ]
                 );
    FullFilePath -> clj_compiler:load_file(FullFilePath)
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
  case 'clojerl.IIndexed':?SATISFIES(Coll) of
    true  -> 'clojerl.IIndexed':nth(Coll, N);
    false -> nth_from(Coll, N)
  end.

-spec nth(any(), integer(), any()) -> any().
nth(?NIL, _, NotFound) -> NotFound;
nth([], _, NotFound)        -> NotFound;
nth(Coll, N, NotFound) ->
  case 'clojerl.IIndexed':?SATISFIES(Coll) of
    true  -> 'clojerl.IIndexed':nth(Coll, N, NotFound);
    false -> nth_from(Coll, N, NotFound)
  end.

-spec nth_from(any(), integer()) -> any().
nth_from(Coll, N) ->
  case 'string?'(Coll) of
    true ->
      case N < 'clojerl.String':count(Coll) of
        true  -> 'clojerl.String':char_at(Coll, N);
        false -> ?ERROR(<<"Index out of bounds">>)
      end;
    _ ->
      case 'clojerl.ISequential':?SATISFIES(Coll) of
        true  -> nth_seq(N, seq(Coll));
        false -> ?ERROR([<<"Can't apply nth to type ">>, type_module(Coll)])
      end
  end.

-spec nth_from(any(), integer(), any()) -> any().
nth_from(Coll, N, NotFound) ->
  case 'string?'(Coll) of
    true ->
      case N < 'clojerl.String':count(Coll) of
        true  -> 'clojerl.String':char_at(Coll, N);
        false -> NotFound
      end;
    _ ->
      case 'clojerl.ISequential':?SATISFIES(Coll) of
        true  -> nth_seq(N, seq(Coll), NotFound);
        false -> ?ERROR([<<"Can't apply nth to type ">>, type_module(Coll)])
      end
  end.

-spec nth_seq(non_neg_integer(), any()) -> any().
nth_seq(Index, Seq) when Index < 0; Seq == ?NIL ->
  ?ERROR(<<"Index out of bounds">>);
nth_seq(0, Seq) ->
  first(Seq);
nth_seq(N, Seq) ->
  nth_seq(N - 1, next(Seq)).

-spec nth_seq(non_neg_integer(), any(), any()) -> any().
nth_seq(Index, Seq, NotFound) when Index < 0; Seq == ?NIL ->
  NotFound;
nth_seq(0, Seq, _) ->
  first(Seq);
nth_seq(N, Seq, NotFound) ->
  nth_seq(N - 1, next(Seq), NotFound).

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
    'clojerl.IEquiv':?SATISFIES(X)
    andalso 'clojerl.IEquiv':?SATISFIES(Y)
  of
    true  -> 'clojerl.IEquiv':equiv(X, Y);
    false -> X =:= Y
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
pop(?NIL)  -> ?NIL;
pop(Stack) -> 'clojerl.IStack':pop(Stack).

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
  'clojerl.IColl':?SATISFIES(X).

-spec 'sequential?'(any()) -> boolean().
'sequential?'(X) ->
  'clojerl.ISequential':?SATISFIES(X).

-spec 'associative?'(any()) -> boolean().
'associative?'(X) ->
  'clojerl.IAssociative':?SATISFIES(X).

-spec 'seq?'(any()) -> boolean().
'seq?'(X) ->
  'clojerl.ISeq':?SATISFIES(X).

-spec 'list?'(any()) -> boolean().
'list?'(X) ->
  type_module(X) == 'clojerl.List'.

-spec 'vector?'(any()) -> boolean().
'vector?'(X) ->
  'clojerl.IVector':?SATISFIES(X).

-spec 'map?'(any()) -> boolean().
'map?'(X) ->
  'clojerl.IMap':?SATISFIES(X).

-spec 'set?'(any()) -> boolean().
'set?'(X) ->
  'clojerl.ISet':?SATISFIES(X).

-spec 'record?'(any()) -> boolean().
'record?'(X) ->
  'clojerl.IRecord':?SATISFIES(X).

-spec 'type?'(any()) -> boolean().
'type?'(X) ->
  'clojerl.IType':?SATISFIES(X).

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
'nil?'(X) -> X =:= ?NIL.

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
  'clojerl.IMeta':?SATISFIES(X).

-spec 'contains?'(any(), any()) -> boolean().
'contains?'(?NIL, _) ->
  false;
'contains?'(Coll, Key)
  when is_number(Key) andalso (is_tuple(Coll) orelse is_binary(Coll)) ->
  N = erlang:trunc(Key),
  Count = count(Coll),
  N >= 0 andalso N < Count;
'contains?'(Coll, Key) ->
  IsAssociative = 'associative?'(Coll),
  IsSet = 'set?'(Coll),

  if
    IsAssociative -> 'clojerl.IAssociative':contains_key(Coll, Key);
    IsSet -> 'clojerl.ISet':contains(Coll, Key);
    true  -> ?ERROR([<<"contains? not supported on type: ">>, type(Coll)])
  end.

-spec get(any(), any()) -> any().
get(?NIL, _Key) -> ?NIL;
get(X, Key) ->
  case 'clojerl.ILookup':?SATISFIES(X) of
    true  -> 'clojerl.ILookup':get(X, Key);
    false -> ?NIL
  end.

-spec get(any(), any(), any()) -> any().
get(?NIL, _Key, NotFound) -> NotFound;
get(X, Key, NotFound) ->
  case 'clojerl.ILookup':?SATISFIES(X) of
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
byte(X) when is_number(X), -128 =< X, X < 127 ->
  erlang:trunc(X);
byte(X) ->
  XStr = str(X),
  ?ERROR(<<"Value out of range for byte: ", XStr/binary>>).

-spec char(integer()) -> binary().
char(X) when is_number(X) ->
  case unicode:characters_to_binary([erlang:trunc(X)], utf8) of
    Char when is_binary(Char) -> Char;
    Error -> error(Error)
  end.

-spec short(number()) -> integer().
short(X) when is_number(X), -32768 =< X, X < 32768 ->
  erlang:trunc(X);
short(X) ->
  XStr = str(X),
  ?ERROR(<<"Value out of range for short: ", XStr/binary>>).

-spec str(any()) -> binary().
str(?NIL) -> <<"">>;
str(X)    -> 'clojerl.IStringable':str(X).

-spec print_str(any()) -> ?NIL.
print_str(X) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(),
  try
    print(X, Writer),
    'erlang.io.StringWriter':str(Writer)
  after
    'erlang.io.StringWriter':close(Writer)
  end.

-spec print(any(), 'erlang.io.IWriter':type()) -> ?NIL.
print(X, Writer) ->
  case 'clojure.core':'print-initialized__val'() of
    true  ->
      'clojure.core':'pr-on'(X, Writer);
    false ->
      PrintReadably = boolean('clojure.core':'*print-readably*__val'()),
      do_print(X, type_module(X), PrintReadably, Writer),
      ?NIL
  end.

-spec do_print(any(), atom(), boolean(), 'erlang.io.IWriter':type()) -> any().
do_print(?NIL, _, _, Writer) ->
  'erlang.io.IWriter':write(Writer, <<"nil">>);
do_print(X, 'clojerl.String', true, Writer) ->
  'erlang.io.IWriter':write(Writer, <<"\"">>),
  do_print_string(X, Writer);
do_print(X, 'clojerl.String', false, Writer) ->
  'erlang.io.IWriter':write(Writer, X);
do_print(X, 'clojerl.Cons', _PrintReadably, Writer) ->
  do_print_seq(X, <<"(">>, <<")">>, Writer);
do_print(X, 'clojerl.LazySeq', _PrintReadably, Writer) ->
  do_print_seq(X, <<"(">>, <<")">>, Writer);
do_print(X, 'clojerl.List', _PrintReadably, Writer) ->
  do_print_seq(X, <<"(">>, <<")">>, Writer);
do_print(X, 'clojerl.Map', _PrintReadably, Writer) ->
  do_print_map(X, Writer);
do_print(X, 'clojerl.Range', _PrintReadably, Writer) ->
  do_print_seq(X, <<"(">>, <<")">>, Writer);
do_print(X, 'clojerl.Set', _PrintReadably, Writer) ->
  do_print_seq(X, <<"#{">>, <<"}">>, Writer);
do_print(X, 'clojerl.SortedSet', _PrintReadably, Writer) ->
  do_print_seq(X, <<"#{">>, <<"}">>, Writer);
do_print(X, 'clojerl.SortedMap', _PrintReadably, Writer) ->
  do_print_map(X, Writer);
do_print(X, 'clojerl.TupleMap', _PrintReadably, Writer) ->
  do_print_map(X, Writer);
do_print(X, 'clojerl.Vector', _PrintReadably, Writer) ->
  do_print_seq(X, <<"[">>, <<"]">>, Writer);
do_print(X, 'clojerl.Subvec', _PrintReadably, Writer) ->
  do_print_seq(X, <<"[">>, <<"]">>, Writer);
do_print(X, 'erlang.List', _PrintReadably, Writer) ->
  do_print_seq(X, <<"#erl(">>, <<")">>, Writer);
do_print(X, 'erlang.Map', _PrintReadably, Writer) ->
  do_print_map(X, <<"#erl{">>, <<"}">>, Writer);
do_print(X, 'erlang.Tuple', _PrintReadably, Writer) ->
  do_print_seq(X, <<"#erl[">>, <<"]">>, Writer);
do_print(X, _Type, _PrintReadably, Writer) ->
  'erlang.io.IWriter':write(Writer, str(X)).

-spec do_print_string(binary(), 'erlang.io.IWriter':type()) -> any().
do_print_string(<<>>, Writer) ->
  'erlang.io.IWriter':write(Writer, <<"\"">>);
do_print_string(<<X/utf8, Rest/binary>>, Writer) ->
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
  'erlang.io.IWriter':write(Writer, Ch),
  do_print_string(Rest, Writer).

-spec do_print_seq(any(), binary(), binary(), 'erlang.io.IWriter':type()) ->
  any().
do_print_seq(X, First, Last, Writer) ->
  'erlang.io.IWriter':write(Writer, First),
  Rest = case to_list(X) of
           [] -> [];
           [Item | Rest_] ->
             print(Item, Writer),
             Rest_
         end,
  PrintItem = fun(Item) ->
                  'erlang.io.IWriter':write(Writer, <<" ">>),
                  print(Item, Writer)
              end,
  lists:foreach(PrintItem, Rest),
  'erlang.io.IWriter':write(Writer, Last).

-spec do_print_map(any(), 'erlang.io.IWriter':type()) -> any().
do_print_map(X, Writer) ->
  do_print_map(X, <<"{">>, <<"}">>, Writer).

-spec do_print_map(any(), binary(), binary(), 'erlang.io.IWriter':type()) ->
  any().
do_print_map(X, First, Last, Writer) ->
  'erlang.io.IWriter':write(Writer, First),
  ItemFun = fun(Item) ->
                [K, V] = to_list(Item),
                print(K, Writer),
                'erlang.io.IWriter':write(Writer, <<" ">>),
                print(V, Writer)
            end,
  ConcatFun = fun(Item) ->
                  'erlang.io.IWriter':write(Writer, <<", ">>),
                  ItemFun(Item)
              end,
  Rest = case to_list(X) of
           [] -> [];
           [Item | Rest_] ->
             ItemFun(Item),
             Rest_
         end,
  lists:foreach(ConcatFun, Rest),
  'erlang.io.IWriter':write(Writer, Last).

-spec 'list'(any()) -> 'clojerl.List':type().
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
    _   -> 'clojerl.Subvec':?CONSTRUCTOR(Vector, Start, End)
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
keys(?NIL) -> ?NIL;
keys(X) ->
  case 'map?'(X) of
    true -> 'clojerl.IMap':keys(X);
    _ ->
      ?ERROR_WHEN( seq(X) =/= ?NIL
                 , [<<"Unable to get keys for: ">>, X]
                 ),
      ?NIL
  end.

-spec vals('clojerl.IMap':type()) -> list().
vals(?NIL) -> ?NIL;
vals(X) ->
  case 'map?'(X) of
    true -> 'clojerl.IMap':vals(X);
    _ ->
      ?ERROR_WHEN( seq(X) =/= ?NIL
                 , [<<"Unable to get vals for: ">>, X]
                 ),
      ?NIL
  end.

-spec 'even?'(integer()) -> boolean().
'even?'(X) ->
  (X band 1) == 0.

-spec apply('clojerl.IFn':type(), 'clojerl.ISequential':type()) -> any().
apply(Fn, Args) ->
  'clojerl.IFn':apply(Fn, Args).

-define(GENSYM_COUNT, '$__gensym_count__$').

-spec reset_id() -> ok.
reset_id() ->
  erlang:erase(?GENSYM_COUNT), ok.

-spec next_id() -> integer().
next_id() ->
  N = case erlang:get(?GENSYM_COUNT) of
        undefined -> 0;
        X -> X
      end,
  erlang:put(?GENSYM_COUNT, N + 1),
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
  fun(X, Y) ->
      case clj_rt:apply(Fun, [X, Y]) of
        Result when is_number(Result)  -> Result < 0;
        Result when is_boolean(Result) -> Result
      end
  end;
compare_fun(Fun, clojure) ->
  fun(X, Y) ->
      case clj_rt:apply(Fun, [X, Y]) of
        Result when is_number(Result) -> Result;
        true -> -1;
        false ->
          case clj_rt:apply(Fun, [Y, X]) of
            true  -> 1;
            false -> 0
          end
      end
  end.

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
  case 'clojerl.IErl':?SATISFIES(X) of
    true  -> 'clojerl.IErl':'->erl'(X, Recursive);
    false -> X
  end.
