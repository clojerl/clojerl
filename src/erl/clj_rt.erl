%% @doc General functions used during runtime.
%%
%% This modules is analougous to the `clojure.lang.RT' class. The
%% modules here are used in places where we can't yet rely on the
%% `clojure.core' being there (e.g. {@link clj_reader}).
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
        , 'clj->erl'/2, 'erl->clj'/2
        ]).

%%------------------------------------------------------------------------------
%% Type functions
%%------------------------------------------------------------------------------

%% @doc Returns the {@link 'erlang.Type'} for `X'.
-spec type(any()) -> 'erlang.Type':type().
type(X) ->
  'erlang.Type':?CONSTRUCTOR(type_module(X)).

%% @doc Returns the module name for `X''s type.
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

%%------------------------------------------------------------------------------
%% Load functions
%%------------------------------------------------------------------------------

%% @equiv load(ScriptBase, true)
-spec load(binary()) -> ?NIL.
load(ScriptBase) ->
  load(ScriptBase, true).

%% @doc Attempts to load a Clojure script.
%%
%% `ScriptBase' is used to resolve the location of a file in the code
%% path with either the `.clje' or `.cljc' extension.
%%
%% When `FailIfNotFound' is `true' generates an error if the file
%% cannot be found.
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

%% @doc Attempts to load a Clojure script.
%%
%% `ScriptBase' is used to resolve the location of a file in the code
%% path.
%%
%% When `FailIfNotFound' is `true' generates an error if the file
%% cannot be found.
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

%% @private
-spec load_ns(binary()) -> ok | error.
load_ns(NsBin) ->
  case code:ensure_loaded(binary_to_atom(NsBin, utf8)) of
    {module, _} -> ok;
    _           -> error
  end.

%% @private
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

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

%% @doc Returns the number of items in the collection.
-spec count(any()) -> integer().
count(?NIL) -> 0;
count(Seq)       -> 'clojerl.ICounted':count(Seq).

%% @doc Returns the value at the index.
-spec nth(any(), integer()) -> any().
nth(?NIL, _) -> ?NIL;
nth([], _) -> ?NIL;
nth(Coll, N) ->
  case 'clojerl.IIndexed':?SATISFIES(Coll) of
    true  -> 'clojerl.IIndexed':nth(Coll, N);
    false -> nth_from(Coll, N)
  end.

%% @doc Returns the value at the index or `NotFound' of `N' is invalid.
-spec nth(any(), integer(), any()) -> any().
nth(?NIL, _, NotFound) -> NotFound;
nth([], _, NotFound)        -> NotFound;
nth(Coll, N, NotFound) ->
  case 'clojerl.IIndexed':?SATISFIES(Coll) of
    true  -> 'clojerl.IIndexed':nth(Coll, N, NotFound);
    false -> nth_from(Coll, N, NotFound)
  end.

%% @private
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

%% @private
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

%% @private
-spec nth_seq(non_neg_integer(), any()) -> any().
nth_seq(Index, Seq) when Index < 0; Seq == ?NIL ->
  ?ERROR(<<"Index out of bounds">>);
nth_seq(0, Seq) ->
  first(Seq);
nth_seq(N, Seq) ->
  nth_seq(N - 1, next(Seq)).

%% @private
-spec nth_seq(non_neg_integer(), any(), any()) -> any().
nth_seq(Index, Seq, NotFound) when Index < 0; Seq == ?NIL ->
  NotFound;
nth_seq(0, Seq, _) ->
  first(Seq);
nth_seq(N, Seq, NotFound) ->
  nth_seq(N - 1, next(Seq), NotFound).

%% @doc Returns true if coll has no items.
-spec 'empty?'(any()) -> boolean().
'empty?'(?NIL) -> true;
'empty?'(Seq)  -> 'clojerl.ISeqable':seq(Seq) == ?NIL.

%% @doc Returns an empty collection of the same category as coll, or nil.
-spec empty(any()) -> any().
empty(Coll) ->
  'clojerl.IColl':empty(Coll).

%% @doc Returns a seq on the collection.
-spec seq(any()) -> any() | ?NIL.
seq(?NIL)    -> ?NIL;
seq(Seqable) -> 'clojerl.ISeqable':seq(Seqable).

%% @doc Returns `Seqable' if applying {@link seq/1} is not `nil',
%% otherwise returns `nil'.
-spec seq_or_else(any()) -> any() | ?NIL.
seq_or_else(Seqable) ->
  case seq(Seqable) of
    ?NIL -> ?NIL;
    _    -> Seqable
  end.

%% @doc Converts the collection into an Erlang list.
-spec to_list(any()) -> [any()].
to_list(?NIL) -> [];
to_list(List) when is_list(List) -> List;
to_list(Seqable) ->
  'clojerl.ISeqable':to_list(Seqable).

%% @doc Equality. Returns true if x equals y, false if not.
-spec equiv(any(), any()) -> boolean().
equiv(X, Y) ->
  case
    'clojerl.IEquiv':?SATISFIES(X)
    andalso 'clojerl.IEquiv':?SATISFIES(Y)
  of
    true  -> 'clojerl.IEquiv':equiv(X, Y);
    false -> X =:= Y
  end.

%% @doc conj[oin]. Returns a new collection with the xs 'added'.
-spec conj(any(), any()) -> any().
conj(?NIL, Item) ->
  list([Item]);
conj(Coll, Item) ->
  'clojerl.IColl':cons(Coll, Item).

%% @doc disj[oin]. Returns a new set of the same (hashed/sorted) type,
%% that does not contain key(s).
-spec disj(any(), any()) -> any().
disj(?NIL, _Item) ->
  ?NIL;
disj(Coll, Item) ->
  'clojerl.ISet':disjoin(Coll, Item).

%% @doc Returns a new seq where x is the first element and seq is the
%% rest.
-spec cons(any(), any()) -> 'clojerl.Cons':type() | 'clojerl.List':type().
cons(Item, ?NIL) ->
  list([Item]);
cons(Item, Seq) ->
  case 'seq?'(Seq) of
    true  -> 'clojerl.Cons':?CONSTRUCTOR(Item, Seq);
    false -> 'clojerl.Cons':?CONSTRUCTOR(Item, seq(Seq))
  end.

%% @doc Returns the first item in the collection.
-spec first(any()) -> any().
first(?NIL) -> ?NIL;
first(Seq) ->
  case 'clojerl.ISeq':?SATISFIES(Seq) of
    true ->
      'clojerl.ISeq':first(Seq);
    false ->
      case seq(Seq) of
        ?NIL -> ?NIL;
        X -> 'clojerl.ISeq':first(X)
      end
  end.

%% @doc Returns a seq of the items after the first.
-spec next(any()) -> any().
next(?NIL) -> ?NIL;
next(Seq) ->
  case 'clojerl.ISeq':?SATISFIES(Seq) of
    true ->
      'clojerl.ISeq':next(Seq);
    false ->
      case seq(Seq) of
        ?NIL -> ?NIL;
        X -> 'clojerl.ISeq':next(X)
      end
  end.

%% @doc Returns a possibly empty seq of the items after the first.
-spec rest(any()) -> any().
rest(?NIL) -> [];
rest(Seq) ->
  case 'clojerl.ISeq':?SATISFIES(Seq) of
    true -> 'clojerl.ISeq':more(Seq);
    false ->
      case seq(Seq) of
        ?NIL -> [];
        X -> 'clojerl.ISeq':more(X)
      end
  end.

%% @doc Same as (first (next x)).
-spec second(any()) -> any().
second(Seq) ->
  first(next(Seq)).

%% @doc Same as (first (next (next x))).
-spec third(any()) -> any().
third(Seq) ->
  first(next(next(Seq))).

%% @doc Same as (first (next (next (next x)))).
-spec fourth(any()) -> any().
fourth(Seq) ->
  first(next(next(next(Seq)))).

%% @doc For a list or queue, same as first, for a vector, same as, but
%% much more efficient than, last. If the collection is empty, returns
%% nil.
-spec peek(any()) -> any().
peek(?NIL) -> ?NIL;
peek(Stack)     -> 'clojerl.IStack':peek(Stack).

%% @doc For a list or queue, returns a new list/queue without the
%% first item, for a vector, returns a new vector without the last
%% item. If the collection is empty, throws an exception. Note - not
%% the same as next/butlast.
-spec pop(any()) -> any().
pop(?NIL)  -> ?NIL;
pop(Stack) -> 'clojerl.IStack':pop(Stack).

%% @doc Returns the name String of a string, symbol or keyword.
-spec name(any()) -> binary() | ?NIL.
name(X) when is_binary(X) -> X;
name(X) -> 'clojerl.INamed':name(X).

%% @doc Returns the namespace String of a symbol or keyword, or nil if
%% not present.
-spec namespace(any()) -> binary() | ?NIL.
namespace(X) ->
  'clojerl.INamed':namespace(X).

%% @doc Returns a Symbol with the given name.
-spec symbol(binary()) -> 'clojerl.Symbol':type().
symbol(Name) ->
  'clojerl.Symbol':?CONSTRUCTOR(Name).

%% @doc Returns a Symbol with the given namespace and name.
-spec symbol(binary(), binary()) -> 'clojerl.Symbol':type().
symbol(Namespace, Name) ->
  'clojerl.Symbol':?CONSTRUCTOR(Namespace, Name).

%% @doc Returns a keyword with the given name.
%%
%% Do not use : in the keyword strings, it will be added automatically.
-spec keyword('clojerl.Symbol':type() | binary()) -> 'clojerl.Keyword':type().
keyword(Name) ->
  'clojerl.Keyword':?CONSTRUCTOR(Name).

%% @doc Returns a keyword with the given name.
%%
%% Do not use : in the keyword strings, it will be added automatically.
-spec keyword(binary(), binary()) -> 'clojerl.Keyword':type().
keyword(Namespace, Name) ->
  'clojerl.Keyword':?CONSTRUCTOR(Namespace, Name).

%% @doc Returns true if `X' implements {@link 'clojerl.IColl'}.
-spec 'coll?'(any()) -> boolean().
'coll?'(X) ->
  'clojerl.IColl':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.ISequential'}.
-spec 'sequential?'(any()) -> boolean().
'sequential?'(X) ->
  'clojerl.ISequential':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.IAssociative'}.
-spec 'associative?'(any()) -> boolean().
'associative?'(X) ->
  'clojerl.IAssociative':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.ISeq'}.
-spec 'seq?'(any()) -> boolean().
'seq?'(X) ->
  'clojerl.ISeq':?SATISFIES(X).

%% @doc Returns true if `X' is a {@link 'clojerl.List'}.
-spec 'list?'(any()) -> boolean().
'list?'(X) ->
  type_module(X) == 'clojerl.List'.

%% @doc Returns true if `X' implements {@link 'clojerl.IVector'}.
-spec 'vector?'(any()) -> boolean().
'vector?'(X) ->
  'clojerl.IVector':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.IMap'}.
-spec 'map?'(any()) -> boolean().
'map?'(X) ->
  'clojerl.IMap':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.ISet'}.
-spec 'set?'(any()) -> boolean().
'set?'(X) ->
  'clojerl.ISet':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.IRecord'}.
-spec 'record?'(any()) -> boolean().
'record?'(X) ->
  'clojerl.IRecord':?SATISFIES(X).

%% @doc Returns true if `X' implements {@link 'clojerl.IType'}.
-spec 'type?'(any()) -> boolean().
'type?'(X) ->
  'clojerl.IType':?SATISFIES(X).

%% @doc Returns true if `X' is a {@link 'clojerl.Symbol'}.
-spec 'symbol?'(any()) -> boolean().
'symbol?'(X) ->
  type_module(X) == 'clojerl.Symbol'.

%% @doc Returns true if `X' is a {@link 'clojerl.Keyword'}.
-spec 'keyword?'(any()) -> boolean().
'keyword?'(X) ->
  type_module(X) == 'clojerl.Keyword'.

%% @doc Returns true if `X' is a number.
-spec 'number?'(any()) -> boolean().
'number?'(X) ->
  type_module(X) == 'clojerl.Integer'
    orelse type_module(X) == 'clojerl.Float'.

%% @doc Returns true if `X' is a BEAM binary with a single UTF-8
%% character.
-spec 'char?'(any()) -> boolean().
'char?'(<<_/utf8>>) -> true;
'char?'(_)          -> false.

%% @doc Returns true if `X' is a {@link 'clojerl.String'}.
-spec 'string?'(any()) -> boolean().
'string?'(X) -> type_module(X) == 'clojerl.String'.

%% @doc Returns true if `X' is nil.
-spec 'nil?'(any()) -> boolean().
'nil?'(X) -> X =:= ?NIL.

%% @doc Returns true if `X' is a {@link 'clojerl.Boolean'}.
-spec 'boolean?'(any()) -> boolean().
'boolean?'(X) -> type_module(X) == 'clojerl.Boolean'.

%% @doc Returns true if `X' is a {@link 'erlang.util.Regex'}.
-spec 'regex?'(any()) -> boolean().
'regex?'(X) -> type_module(X) == 'erlang.util.Regex'.

%% @doc Returns true if `X' is a {@link 'clojerl.Var'}.
-spec 'var?'(any()) -> boolean().
'var?'(X) ->
  type_module(X) == 'clojerl.Var'.

%% @doc Fetches the value referenced by `X'.
%%
%% `X' needs to implement {@link 'clojerl.IDeref'} (e.g. {@link 'clojerl.Var'},
%% {@link 'clojerl.Atom'} or {@link 'clojerl.Agent'}).
-spec deref(any()) -> any().
deref(X) ->
  'clojerl.IDeref':deref(X).

%% @doc Changes the dynamically bound value for `Var'.
-spec 'set!'('clojerl.Var':type(), any()) -> any().
'set!'(Var, Value) ->
  'clojerl.Var':dynamic_binding(Var, Value).

%% @doc Returns the metadata of obj, returns nil if there is no
%% metadata.
-spec meta(any()) -> any().
meta(X) ->
  'clojerl.IMeta':meta(X).

%% @doc Returns an object of the same type and value as `X', with map
%% `Meta' as its metadata.
-spec with_meta(T, map()) -> T.
with_meta(X, Meta) ->
  'clojerl.IMeta':with_meta(X, Meta).

%% @doc Returns true if `X' implements {@link 'clojerl.IMeta'}.
-spec 'meta?'(any()) -> any().
'meta?'(X) ->
  'clojerl.IMeta':?SATISFIES(X).

%% @doc Returns true if key is present in the given collection,
%% otherwise returns false.
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

%% @doc Returns the value mapped to `Key', or nil if `Key' is not
%% present.
-spec get(any(), any()) -> any().
get(?NIL, _Key) -> ?NIL;
get(X, Key) ->
  case 'clojerl.ILookup':?SATISFIES(X) of
    true  -> 'clojerl.ILookup':get(X, Key);
    false -> ?NIL
  end.

%% @doc Returns the value mapped to `Key', or `NotFound' if `Key' is not
%% present.
-spec get(any(), any(), any()) -> any().
get(?NIL, _Key, NotFound) -> NotFound;
get(X, Key, NotFound) ->
  case 'clojerl.ILookup':?SATISFIES(X) of
    true  -> 'clojerl.ILookup':get(X, Key, NotFound);
    false -> NotFound
  end.

%% @doc assoc[iate]. When applied to a map, returns a new map of the
%% same (hashed/sorted) type, that contains the mapping of key(s) to
%% val(s). When applied to a vector, returns a new vector that
%% contains val at index.
-spec assoc('clojerl.IAssociative':type(), any(), any()) ->
  'clojerl.IAssociative':type().
assoc(?NIL, Key, Value) ->
  hash_map([Key, Value]);
assoc(Map, Key, Value) ->
  'clojerl.IAssociative':assoc(Map, Key, Value).

%% @doc dissoc[iate]. Returns a new map of the same (hashed/sorted)
%% type, that does not contain a mapping for key(s).
-spec dissoc('clojerl.IMap':type(), any()) -> 'clojerl.IMap':type().
dissoc(?NIL, _Key) ->
  ?NIL;
dissoc(Map, Key) ->
  'clojerl.IMap':without(Map, Key).

%% @doc Returns the map entry for key, or nil if key not present.
-spec find(any(), any()) -> any().
find(?NIL, _) ->
  ?NIL;
find(Map, Key) ->
  case 'associative?'(Map) of
    true  -> 'clojerl.IAssociative':entry_at(Map, Key);
    false -> ?NIL
  end.

%% @doc Returns a map that consists of the rest of the maps conj-ed
%% onto the first.
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

%% @doc Coerce to boolean.
-spec boolean(any()) -> boolean().
boolean(?NIL) -> false;
boolean(false) -> false;
boolean(_) -> true.

%% @doc Coerce to byte.
-spec byte(number()) -> integer().
byte(X) when is_number(X), -128 =< X, X < 128 ->
  erlang:trunc(X);
byte(X) ->
  XStr = str(X),
  ?ERROR(<<"Value out of range for byte: ", XStr/binary>>).

%% @doc Coerce to char.
-spec char(integer()) -> binary().
char(X) when is_number(X) ->
  case unicode:characters_to_binary([erlang:trunc(X)], utf8) of
    Char when is_binary(Char) -> Char;
    Error -> error(Error)
  end.

%% @doc Coerce to short.
-spec short(number()) -> integer().
short(X) when is_number(X), -32768 =< X, X < 32768 ->
  erlang:trunc(X);
short(X) ->
  XStr = str(X),
  ?ERROR(<<"Value out of range for short: ", XStr/binary>>).

%% @doc Returns the string representation of `X' by applying {@link
%% 'clojerl.IStringable':str/1}.
-spec str(any()) -> binary().
str(?NIL) -> <<"">>;
str(X)    -> 'clojerl.IStringable':str(X).

%% @doc Returns the string representation of `X' using
%% `clojure.core/pr-on' when initialized or a fallback implementation
%% otherwise.
-spec print_str(any()) -> ?NIL.
print_str(X) ->
  Writer = 'erlang.io.StringWriter':?CONSTRUCTOR(),
  try
    print(X, Writer),
    'erlang.io.StringWriter':str(Writer)
  after
    'erlang.io.StringWriter':close(Writer)
  end.

%% @doc Prints `X' to `Writer` using `clojure.core/pr-on' when
%% initialized or a fallback implementation otherwise.
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

%% @private
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

%% @private
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

%% @private
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

%% @private
-spec do_print_map(any(), 'erlang.io.IWriter':type()) -> any().
do_print_map(X, Writer) ->
  do_print_map(X, <<"{">>, <<"}">>, Writer).

%% @private
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

%% @doc Creates a new list containing the items.
-spec 'list'(any()) -> 'clojerl.List':type().
list(Items) ->
  'clojerl.List':?CONSTRUCTOR(Items).

%% @doc Creates a new vector containing the items.
-spec vector(list()) -> 'clojerl.Vector':type().
vector(Items) when is_list(Items) ->
  'clojerl.Vector':?CONSTRUCTOR(Items);
vector(Items) ->
  vector(to_list(Items)).

%% @doc Returns a persistent vector of the items in vector from start
%% (inclusive) to end (exclusive).
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

%% @doc Returns a new hash map with supplied mappings.
-spec hash_map(list()) -> 'clojerl.Map':type().
hash_map(Items) ->
  case count(Items) of
    0 -> 'clojerl.Map':?CONSTRUCTOR([]);
    _ -> 'clojerl.Map':?CONSTRUCTOR(seq(Items))
  end.

%% @doc Returns a new hash set with supplied items.
-spec hash_set(list()) -> 'clojerl.Set':type().
hash_set(Items) ->
  case count(Items) of
    0 -> 'clojerl.Set':?CONSTRUCTOR([]);
    _ -> 'clojerl.Set':?CONSTRUCTOR(seq(Items))
  end.

%% @doc Returns a sequence of the map's keys, in the same order as
%% (seq map).
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

%% @doc Returns a sequence of the map's values, in the same order as (seq map).
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

%% @doc Returns true if n is even, throws an exception if n is not an integer
-spec 'even?'(integer()) -> boolean().
'even?'(X) ->
  (X band 1) == 0.

%% @doc Applies the argument to the functions assuming `Fn' implements
%% {@link 'clojerl.IFn'}.
-spec apply('clojerl.IFn':type(), 'clojerl.ISequential':type()) -> any().
apply(Fn, Args) ->
  'clojerl.IFn':apply(Fn, Args).

-define(GENSYM_COUNT, '$__gensym_count__$').

%% @doc Resets this local process gensym counter.
-spec reset_id() -> ok.
reset_id() ->
  erlang:erase(?GENSYM_COUNT), ok.

%% @doc Returns the next id from the local process gensym counter.
-spec next_id() -> integer().
next_id() ->
  N = case erlang:get(?GENSYM_COUNT) of
        undefined -> 0;
        X -> X
      end,
  erlang:put(?GENSYM_COUNT, N + 1),
  N.

%% @equiv gensym(<<"G__">>)
-spec gensym() -> 'clojer.Symbol':type().
gensym() ->
  gensym(<<"G__">>).

%% @doc Generates a new symbol unique to the current process using
%% `Prefix'.
-spec gensym(binary()) -> 'clojer.Symbol':type().
gensym(Prefix) ->
  PartsBin = [Prefix, integer_to_list(next_id())],
  symbol(iolist_to_binary(PartsBin)).

%% @doc Converts the provided `Fun' into a compatible `erlang' or
%% `clojure' compare function.
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

%% @doc Return a random permutation of coll
-spec shuffle(any()) -> [any()].
shuffle(Seq) ->
  Items = [{rand:uniform(), X} || X <- to_list(Seq)],
  [X || {_, X} <- lists:sort(Items)].

%% @doc Returns the hash code of its argument.
-spec hash(any()) -> integer().
hash(?NIL) -> 0;
hash(X)    -> 'clojerl.IHash':hash(X).

%% @doc Returns the Erlang representation of x when it implements
%% IEncodeErlang.
-spec 'clj->erl'(any(), boolean()) -> any().
'clj->erl'(?NIL, _)      -> ?NIL;
'clj->erl'(X, Recursive) ->
  case 'clojerl.IEncodeErlang':?SATISFIES(X) of
    true  -> 'clojerl.IEncodeErlang':'clj->erl'(X, Recursive);
    false -> X
  end.

%% @doc "Returns the Clojure representation of x when it implements
%% IEncodeClojure.
-spec 'erl->clj'(any(), boolean()) -> any().
'erl->clj'(?NIL, _)      -> ?NIL;
'erl->clj'(X, Recursive) ->
  case 'clojerl.IEncodeClojure':?SATISFIES(X) of
    true  -> 'clojerl.IEncodeClojure':'erl->clj'(X, Recursive);
    false -> X
  end.
