-module(clj_reader_SUITE).

-include("clojerl.hrl").
-include("clojerl_int.hrl").
-include("clj_test_utils.hrl").
-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , groups/0
        , init_per_group/2
        , end_per_group/2
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ eof/1
        , number/1
        , string/1
        , keyword/1
        , symbol/1
        , comment/1
        , quote/1
        , deref/1
        , meta/1
        , syntax_quote/1
        , unquote/1
        , list/1
        , vector/1
        , map/1
        , set/1
        , unmatched_delim/1
        , char/1
        , arg/1
        , fn/1
        , eval/1
        , var/1
        , regex/1
        , unreadable_form/1
        , discard/1
        , 'cond'/1
        , unsupported_reader/1
        , erl_literals/1
        , erl_binary/1
        , erl_alias/1
        , tagged/1
        ]).

-spec all() -> [atom()].
all() -> [{group, read}, {group, read_io}].

-spec groups() -> [atom()].
groups() ->
  [ {read, [], clj_test_utils:all(?MODULE)}
  , {read_io, [], clj_test_utils:all(?MODULE)}
  ].

-spec init_per_group(atom(), config()) -> config().
init_per_group(read, Config) ->
  [ {read_fun, fun read/1}
  , {read_fun2, fun read/2}
  , {read_all_fun, fun read_all/1}
  | Config
  ];
init_per_group(read_io, Config) ->
  [ {read_fun, fun read_io/1}
  , {read_fun2, fun read_io/2}
  , {read_all_fun, fun read_all_io/1}
  | Config
  ].

-spec end_per_group(atom(), config()) -> ok.
end_per_group(_, _Config) -> ok.

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

eof(Config) ->
  ReadFun = ?config(read_fun, Config),

  ct:comment("Read empty binary"),
  ok = try ReadFun(<<"">>)
       catch _:_ -> ok end,

  ok = try ReadFun(<<" , , , , ">>)
       catch _:_ -> ok end,

  ok = try ReadFun(<<", , , \t \n ,, ">>)
       catch _:_ -> ok end,

  {comments, ""}.

number(Config) ->
  ReadFun = ?config(read_fun, Config),
  ReadAllFun = ?config(read_all_fun, Config),

  0   = ReadFun(<<"0">>),
  0.0 = ReadFun(<<"0.0">>),

  1    = ReadFun(<<"1">>),
  1    = ReadFun(<<"+1">>),
  -1   = ReadFun(<<"-1">>),
  1.0  = ReadFun(<<"1.0">>),
  1.0  = ReadFun(<<"10E-1">>),
  1.0  = ReadFun(<<"10.0e-1">>),
  1.0  = ReadFun(<<"10.e-1">>),
  1.0  = ReadFun(<<"10e-1">>),
  1.0  = ReadFun(<<"+10e-1">>),
  -1.0 = ReadFun(<<"-10e-1">>),

  42 = ReadFun(<<"0x2A">>),
  42 = ReadFun(<<"052">>),
  42 = ReadFun(<<"36r16">>),
  42 = ReadFun(<<"36R16">>),

  ok = try ReadFun(<<"1/2">>), error
       catch _:_ -> ok
       end,

  ok = try ReadFun(<<"12A">>), error
       catch _:_ -> ok
       end,

  [0, 1, 2.0, 3.0e-10] = ReadAllFun(<<"0N 1 2.0 3e-10">>),

  {comments, ""}.

string(Config) ->
  ReadFun = ?config(read_fun, Config),

  <<"">> = ReadFun(<<"\"\"">>),

  <<"hello">> = ReadFun(<<"\"hello\"">>),

  <<"hello \t world!">> = ReadFun(<<"\"hello \\t world!\"">>),
  <<"hello \n world!">> = ReadFun(<<"\"hello \\n world!\"">>),
  <<"hello \r world!">> = ReadFun(<<"\"hello \\r world!\"">>),
  <<"hello \" world!">> = ReadFun(<<"\"hello \\\" world!\"">>),
  <<"hello \f world!">> = ReadFun(<<"\"hello \\f world!\"">>),
  <<"hello \b world!">> = ReadFun(<<"\"hello \\b world!\"">>),
  <<"hello \\ world!">> = ReadFun(<<"\"hello \\\\ world!\"">>),

  <<"hello © world!"/utf8>> =
    ReadFun(<<"\"hello \\u00A9 world!\"">>),
  ok = try ReadFun(<<"\"hello \\u00A world!\"">>)
       catch _:_ -> ok
       end,
  ok = try ReadFun(<<"\"hello \\u00Z world!\"">>)
       catch _:_ -> ok
       end,

  <<"hello © world!"/utf8>> =
    ReadFun(<<"\"hello \\251 world!\"">>),

  ct:comment("EOF"),
  ok = try ReadFun(<<"\"hello world!">>)
       catch _:_ -> ok
       end,

  ct:comment("Octal not in range"),
  ok = try ReadFun(<<"\"hello \\400 world!">>)
       catch _:_ -> ok
       end,

  ct:comment("Number not in base"),
  ok = try ReadFun(<<"\"hello \\u000Z world!">>)
       catch _:_ -> ok
       end,

  ct:comment("Unsupported escaped char"),
  ok = try ReadFun(<<"\"hello \\z world!">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

keyword(Config) ->
  ReadFun = ?config(read_fun, Config),

  FooSym   = clj_rt:symbol(<<"foo">>),
  FooNs    = 'clojerl.Namespace':find_or_create(FooSym),
  SomeSym  = clj_rt:symbol(<<"some">>),
  SomeNs   = 'clojerl.Namespace':find_or_create(SomeSym),

  Keyword1 = clj_rt:keyword(<<"hello-world">>),
  Keyword1 = ReadFun(<<":hello-world">>),

  Keyword2 = clj_rt:keyword(<<"some">>, <<"hello-world">>),
  Keyword2 = ReadFun(<<"::hello-world">>),

  Keyword3 = clj_rt:keyword(<<"another-ns">>, <<"hello-world">>),
  Keyword3 = ReadFun(<<":another-ns/hello-world">>),

  Keyword4 = clj_rt:keyword(<<"/">>),
  Keyword4 = ReadFun(<<":/">>),

  Keyword5 = clj_rt:keyword(<<"some">>, <<"/">>),
  Keyword5 = ReadFun(<<":some//">>),

  FSym     = clj_rt:symbol(<<"f">>),
  SomeNs   = 'clojerl.Namespace':add_alias(SomeNs, FSym, FooNs),
  Keyword6 = clj_rt:keyword(<<"foo">>, <<"hello-world">>),
  Keyword6 = ReadFun(<<"::f/hello-world">>),

  Keyword7 = clj_rt:keyword(<<"42hello-world">>),
  Keyword7 = ReadFun(<<":42hello-world">>),

  ct:comment("Error: triple colon :::"),
  ok = try ReadFun(<<":::hello-world">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: empty name after namespace"),
  ok = try ReadFun(<<":some-ns/">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: colon as last char"),
  ok = try ReadFun(<<":hello-world:">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: single colon"),
  ok = try ReadFun(<<":">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

symbol(Config) ->
  ReadFun = ?config(read_fun, Config),

  Symbol1 = clj_rt:symbol(<<"hello-world">>),
  ?assertEquiv(ReadFun(<<"hello-world">>), Symbol1),

  Symbol2 = clj_rt:symbol(<<"some-ns">>, <<"hello-world">>),
  ?assertEquiv(ReadFun(<<"some-ns/hello-world">>), Symbol2),

  Symbol3 = clj_rt:symbol(<<"another-ns">>, <<"hello-world">>),
  ?assertEquiv( ReadFun(<<"another-ns/hello-world">>)
              , Symbol3
              ),

  Symbol4 = clj_rt:symbol(<<"some-ns">>, <<"/">>),
  ?assertEquiv(ReadFun(<<"some-ns//">>), Symbol4),

  ct:comment("nil, true & false"),
  ?NIL = ReadFun(<<"nil">>),
  true = ReadFun(<<"true">>),
  false = ReadFun(<<"false">>),

  ct:comment("Error: empty name after namespace"),
  ok = try ReadFun(<<"some-ns/">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: colon as last char"),
  ok = try ReadFun(<<"hello-world:">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: numeric first char in name"),
  ok = try ReadFun(<<"42hello-world">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

comment(Config) ->
  ReadAllFun = ?config(read_all_fun, Config),

  BlaKeyword = clj_rt:keyword(<<"bla">>),

  ct:comment("Single semi-colon"),
  [1, BlaKeyword] = ReadAllFun(<<"1 ; comment\n :bla ">>),

  ct:comment("Two semi-colon"),
  [1, BlaKeyword] = ReadAllFun(<<"1 ;; comment\n :bla ">>),

  ct:comment("A bunch of semi-colons"),
  [1, BlaKeyword] = ReadAllFun(<<"1 ;;;; comment\n :bla ">>),

  ct:comment("Comment reader"),
  [1, BlaKeyword] = ReadAllFun(<<"1 #! comment\n :bla ">>),

  ct:comment("Comment after meta"),
  [BarSym1] = ReadAllFun(<<"^:foo ;; comment\n bar">>),
  <<"bar">> = clj_rt:name(BarSym1),
  true      = clj_rt:get(clj_rt:meta(BarSym1), foo),

  [_, BarSym2] = ReadAllFun(<<"hello ^:foo ;; comment\n bar">>),
  <<"bar">> = clj_rt:name(BarSym2),
  true      = clj_rt:get(clj_rt:meta(BarSym2), foo),

  {comments, ""}.

quote(Config) ->
  ReadFun = ?config(read_fun, Config),

  QuoteSymbol = clj_rt:symbol(<<"quote">>),
  ListSymbol = clj_rt:symbol(<<"list">>),

  ct:comment("Quote number"),
  ListQuote1 = clj_rt:list([QuoteSymbol, 1]),
  ListQuote1 = ReadFun(<<"'1">>),

  ct:comment("Quote symbol"),
  ListQuote2 = clj_rt:list([QuoteSymbol, ListSymbol]),
  ?assertEquiv(ReadFun(<<"'list">>), ListQuote2),

  ct:comment("Quote space symbol"),
  ?assertEquiv(ReadFun(<<"' list">>), ListQuote2),

  ct:comment("Error: only provide ' "),
  ok = try ReadFun(<<"'">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

deref(Config) ->
  ReadFun = ?config(read_fun, Config),
  ReadAllFun = ?config(read_all_fun, Config),

  DerefSymbol = clj_rt:symbol(<<"clojure.core">>, <<"deref">>),
  ListSymbol = clj_rt:symbol(<<"list">>),

  ct:comment("Deref number :P"),
  ListDeref1 = clj_rt:list([DerefSymbol, 1]),
  ListDeref1= ReadFun(<<"@1">>),

  ct:comment("Deref symbol :P"),
  ListDeref2 = clj_rt:list([DerefSymbol, ListSymbol]),
  ?assertEquiv(ReadFun(<<"@list">>), ListDeref2),

  ct:comment("Deref symbol :P and read other stuff"),
  [ListDeref3, 42.0] = ReadAllFun(<<"@list 42.0">>),
  ?assertEquiv(ListDeref3, ListDeref2),

  ct:comment("Error: only provide @ "),
  ok = try ReadFun(<<"@">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

meta(Config) ->
  ReadFun = ?config(read_fun, Config),

  MetadataKw = ReadFun(<<"{:private true}">>),
  MetadataSym = ReadFun(<<"{:tag private}">>),

  ct:comment("Keyword doesn't support metadata"),
  ok = try ReadFun(<<"^:private :hello">>), error
       catch _:_ -> ok
       end,

  %% Compare metadata after removing the location information.
  AssertMetaEquiv =
    fun(Value, ExpectedMeta) ->
        Meta = clj_rt:meta(Value),
        MetaNoLocation = clj_reader:remove_location(Meta),
        ?assertEquiv(MetaNoLocation, ExpectedMeta)
    end,

  ct:comment("Keyword meta to symbol"),
  SymbolWithMetaKw = ReadFun(<<"^:private hello">>),
  AssertMetaEquiv(SymbolWithMetaKw, MetadataKw),

  ct:comment("Symbol meta to symbol"),
  SymbolWithMetaSym = ReadFun(<<"^private hello">>),
  AssertMetaEquiv(SymbolWithMetaSym, MetadataSym),

  ct:comment("Map meta to symbol"),
  MapWithMetaKw = ReadFun(<<"^:private {}">>),
  AssertMetaEquiv(MapWithMetaKw, MetadataKw),

  ct:comment("Map meta to symbol"),
  MapWithMetaSym = ReadFun(<<"^private {}">>),
  AssertMetaEquiv(MapWithMetaSym, MetadataSym),

  ct:comment("List meta to symbol"),
  ListWithMetaKw = ReadFun(<<"^:private ()">>),
  AssertMetaEquiv(ListWithMetaKw, MetadataKw),

  ct:comment("List meta to symbol"),
  ListWithMetaSym = ReadFun(<<"^private ()">>),
  AssertMetaEquiv(ListWithMetaSym, MetadataSym),

  ct:comment("Vector meta to symbol"),
  VectorWithMetaKw = ReadFun(<<"^:private []">>),
  AssertMetaEquiv(VectorWithMetaKw, MetadataKw),

  ct:comment("Vector meta to symbol"),
  VectorWithMetaSym = ReadFun(<<"^private []">>),
  AssertMetaEquiv(VectorWithMetaSym, MetadataSym),

  ct:comment("Set meta to symbol"),
  SetWithMetaKw = ReadFun(<<"^:private #{}">>),
  AssertMetaEquiv(SetWithMetaKw, MetadataKw),

  ct:comment("Set meta to symbol"),
  SetWithMetaSym = ReadFun(<<"^private #{}">>),
  AssertMetaEquiv(SetWithMetaSym, MetadataSym),

  ct:comment("Meta number"),
  ok = try ReadFun(<<"^1 1">>)
       catch _:_ -> ok
       end,

  ct:comment("Reader meta number"),
  ok = try ReadFun(<<"#^1 1">>)
       catch _:_ -> ok
       end,

  ct:comment("Meta without form"),
  ok = try ReadFun(<<"^:private">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

syntax_quote(Config) ->
  ReadFun = ?config(read_fun, Config),

  QuoteSym = clj_rt:symbol(<<"quote">>),
  QuoteFun = fun(X) -> clj_rt:list([QuoteSym, X]) end,

  ct:comment("Read special form"),
  DoSym = clj_rt:symbol(<<"do">>),
  QuoteDoList = QuoteFun(DoSym),
  DoSyntaxQuote = ReadFun(<<"`do">>),
  ?assertEquiv(DoSyntaxQuote, QuoteDoList),

  ct:comment("Form with meta info other than location keeps meta"),
  WithMetaSym = clj_rt:symbol(<<"clojure.core">>, <<"with-meta">>),
  DoWithMetaSyntaxQuote = ReadFun(<<"`^:foo do">>),
  ?assertEquiv(clj_rt:first(DoWithMetaSyntaxQuote), WithMetaSym),
  ?assertEquiv(clj_rt:second(DoWithMetaSyntaxQuote), QuoteDoList),

  DefSym = clj_rt:symbol(<<"def">>),

  QuoteDefList = clj_rt:list([QuoteSym, DefSym]),
  DefSyntaxQuote = ReadFun(<<"`def">>),
  ?assertEquiv(DefSyntaxQuote, QuoteDefList),

  ct:comment("Read literals"),
  1 = ReadFun(<<"`1">>),
  42.0 = ReadFun(<<"`42.0">>),
  <<"something!">> = ReadFun(<<"`\"something!\"">>),

  ct:comment("Keywords can't have metadata"),
  HelloKeyword = clj_rt:keyword(<<"hello">>),
  HelloKeyword = ReadFun(<<"`:hello">>),

  ct:comment("Read values that can have metadata"),

  ct:comment("Read unqualified symbol"),
  UserHelloSym = clj_rt:symbol(<<"clojure.core">>, <<"hello">>),
  HelloSyntaxQuote = ReadFun(<<"`hello">>),
  ?assertEquiv(HelloSyntaxQuote, QuoteFun(UserHelloSym)),

  ct:comment("Read qualified symbol"),
  SomeNsHelloSym = clj_rt:symbol(<<"some-ns">>, <<"hello">>),
  SomeNsHelloSyntaxQuote = ReadFun(<<"`some-ns/hello">>),
  ?assertEquiv( SomeNsHelloSyntaxQuote, QuoteFun(SomeNsHelloSym)),

  ct:comment("Read auto-gen symbol"),
  ListGenSym = ReadFun(<<"`hello#">>),
  QuotedGenSym = clj_rt:second(ListGenSym),
  GenSymName = clj_rt:name(QuotedGenSym),
  {match, _} = re:run(GenSymName, "hello__\\d+__auto__"),

  ct:comment("Read auto-gen symbol, "
             "check generated symbols have the same name"),
  ListApply  = ReadFun(<<"`(hello# hello# world#)">>),
  ListConcat = clj_rt:third(ListApply),
  ListSecond = clj_rt:second(ListConcat),
  ListThird  = clj_rt:third(ListConcat),
  ?assertEquiv(clj_rt:second(ListSecond), clj_rt:second(ListThird)),

  ct:comment("Read unquote"),
  HelloSym = clj_rt:symbol(<<"hello">>),
  ListWithMetaHelloSym = ReadFun(<<"`~hello">>),
  ?assertEquiv(ListWithMetaHelloSym, HelloSym),

  ct:comment("Use unquote splice not in list"),
  ok = try ReadFun(<<"`~@(hello)">>)
       catch _:_ -> ok end,

  ct:comment("Read list and empty list"),
  ListHello = ReadFun(<<"`(hello :world)">>),
  ListHelloCheck = ReadFun(<<"(clojure.core/concat"
                             "  (clojure.core/list 'clojure.core/hello)"
                             "  (clojure.core/list :world))">>),
  ?assertEquiv( clj_rt:third(ListHello), ListHelloCheck),

  EmptyList = ReadFun(<<"`()">>),
  EmptyListCheck = ReadFun(<<"(clojure.core/list)">>),
  ?assertEquiv(clj_rt:third(EmptyList), EmptyListCheck),

  ct:comment("Read map"),
  Map = ReadFun(<<"`{hello :world}">>),
  MapCheck = ReadFun(<<"(clojure.core/apply"
                       "  clojure.core/hash-map"
                       "  (clojure.core/concat"
                       "    (clojure.core/list 'clojure.core/hello)"
                       "    (clojure.core/list :world)))">>),
  ?assertEquiv(Map, MapCheck),

  ct:comment("Read vector"),
  Vector = ReadFun(<<"`[hello :world]">>),
  VectorCheck = ReadFun(<<"(clojure.core/apply"
                          "  clojure.core/vector"
                          "  (clojure.core/concat"
                          "    (clojure.core/list 'clojure.core/hello)"
                          "    (clojure.core/list :world)))">>),
  ?assertEquiv(Vector, VectorCheck),

  ct:comment("Read set"),
  Set = ReadFun(<<"`#{hello :world}">>),
  SetCheck = ReadFun(<<"(clojure.core/apply"
                       "  clojure.core/hash-set"
                       "  (clojure.core/concat"
                       "    (clojure.core/list 'clojure.core/hello)"
                       "    (clojure.core/list :world)))">>),
  ?assertEquiv(Set, SetCheck),

  ct:comment("Read unquote-splice inside list"),
  HelloWorldSup = ReadFun(<<"`(~@(hello world) :sup?)">>),
  HelloWorldSupCheck = ReadFun(<<"(clojure.core/concat"
                                 "  (hello world)"
                                 "  (clojure.core/list :sup?))">>),
  ?assertEquiv(clj_rt:third(HelloWorldSup), HelloWorldSupCheck),

  ct:comment("Read unquote inside list"),
  ListHelloWorld = ReadFun(<<"`(~hello :world)">>),
  ListHelloWorldCheck = ReadFun(<<"(clojure.core/concat"
                                  "  (clojure.core/list hello)"
                                  "  (clojure.core/list :world))">>),
  ?assertEquiv(clj_rt:third(ListHelloWorld), ListHelloWorldCheck),

  ct:comment("Syntax quote a symbol with dots"),
  HelloWorldWithDot = ReadFun(<<"`hello.world">>),
  HelloWorldWithDotCheck = ReadFun(<<"(quote hello.world)">>),
  ?assertEquiv(HelloWorldWithDot, HelloWorldWithDotCheck),

  ct:comment("Generated symbol in nested structure keeps meta"),
  FooSymbol  = clj_rt:symbol(<<"foo">>),
  GenSym0    = ReadFun(<<"`(^foo w#)">>),
  GenSym1    = clj_rt:third(GenSym0),
  GenSym2    = clj_rt:second(GenSym1),
  GenSym3    = clj_rt:second(clj_rt:second(GenSym2)),
  GenSymMeta = clj_rt:meta(GenSym3),
  ?assertEquiv(clj_rt:get(GenSymMeta, tag), FooSymbol),

  {comments, ""}.

unquote(Config) ->
  ReadFun = ?config(read_fun, Config),

  UnquoteSymbol = clj_rt:symbol(<<"clojure.core">>, <<"unquote">>),
  UnquoteSplicingSymbol = clj_rt:symbol(<<"clojure.core">>,
                                               <<"unquote-splicing">>),
  HelloWorldSymbol = clj_rt:symbol(<<"hello-world">>),

  ct:comment("Unquote"),
  ListUnquote = clj_rt:list([UnquoteSymbol, HelloWorldSymbol]),
  ?assertEquiv(ReadFun(<<"~hello-world">>), ListUnquote),

  ct:comment("Unquote splicing"),
  ListUnquoteSplicing = clj_rt:list([UnquoteSplicingSymbol,
                                       HelloWorldSymbol]),
  ?assertEquiv(ReadFun(<<"~@hello-world">>), ListUnquoteSplicing),

  ct:comment("Unquote nothing"),
  ok = try ReadFun(<<"~">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

list(Config) ->
  ReadFun = ?config(read_fun, Config),

  HelloWorldKeyword = clj_rt:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_rt:symbol(<<"hello-world">>),

  ct:comment("Empty List"),
  EmptyList = clj_rt:list([]),
  ?assertEquiv(ReadFun(<<"()">>), EmptyList),

  ct:comment("List"),
  List = clj_rt:list([HelloWorldKeyword, HelloWorldSymbol]),
  ?assertEquiv( ReadFun(<<"(:hello-world hello-world)">>)
                       , List
                       ),

  ct:comment("List & space"),
  ?assertEquiv( ReadFun(<<"(:hello-world hello-world )">>)
                       , List
                       ),

  ct:comment("List without closing paren"),
  ok = try ReadFun(<<"(1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

vector(Config) ->
  ReadFun = ?config(read_fun, Config),

  HelloWorldKeyword = clj_rt:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_rt:symbol(<<"hello-world">>),

  ct:comment("Vector"),
  Vector = 'clojerl.Vector':?CONSTRUCTOR([HelloWorldKeyword, HelloWorldSymbol]),
  ?assertEquiv( ReadFun(<<"[:hello-world hello-world]">>)
                       , Vector
                       ),

  ct:comment("Vector without closing bracket"),
  ok = try ReadFun(<<"[1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

map(Config) ->
  ReadFun = ?config(read_fun, Config),

  HelloWorldKeyword = clj_rt:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_rt:symbol(<<"hello-world">>),

  ct:comment("Map"),
  Map = 'clojerl.Map':?CONSTRUCTOR([ HelloWorldKeyword
                                   , HelloWorldSymbol
                                   , HelloWorldSymbol
                                   , HelloWorldKeyword
                                   ]
                                  ),
  MapResult = ReadFun(<<"{:hello-world hello-world,"
                        " hello-world :hello-world}">>),
  ?assertEquiv(Map, MapResult),

  ct:comment("Map without closing braces"),
  ok = try ReadFun(<<"{1 42.0">>)
       catch _:_ -> ok
       end,

  ct:comment("Literal map with odd number of expressions"),
  ok = try ReadFun(<<"{1 42.0 :a}">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

set(Config) ->
  ReadFun = ?config(read_fun, Config),

  HelloWorldKeyword = clj_rt:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_rt:symbol(<<"hello-world">>),

  ct:comment("Set"),
  Set = 'clojerl.Set':?CONSTRUCTOR([HelloWorldKeyword, HelloWorldSymbol]),
  SetResult = ReadFun(<<"#{:hello-world hello-world}">>),
  ?assertEquiv(Set, SetResult),

  ct:comment("Set without closing braces"),
  ok = try ReadFun(<<"#{1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

unmatched_delim(Config) ->
  ReadAllFun = ?config(read_all_fun, Config),

  ct:comment("Single closing paren"),
  ok = try ReadAllFun(<<"{1 42.0} )">>)
       catch _:_ -> ok
       end,

  ct:comment("Single closing bracket"),
  ok = try ReadAllFun(<<"{1 42.0} ]">>)
       catch _:_ -> ok
       end,

  ct:comment("Single closing braces"),
  ok = try ReadAllFun(<<"{1 42.0} } ">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

char(Config) ->
  ReadFun = ?config(read_fun, Config),
  ReadAllFun = ?config(read_all_fun, Config),

  ct:comment("Read single char"),
  <<"a">>  = ReadFun(<<"\\a">>),

  <<"\n">> = ReadFun(<<"\\newline">>),
  <<" ">>  = ReadFun(<<"\\space">>),
  <<"\t">> = ReadFun(<<"\\tab">>),
  <<"\b">> = ReadFun(<<"\\backspace">>),
  <<"\f">> = ReadFun(<<"\\formfeed">>),
  <<"\r">> = ReadFun(<<"\\return">>),

  <<"©"/utf8>> = ReadFun(<<"\\u00A9">>),
  <<"ß"/utf8>> = ReadFun(<<"\\o337">>),

  <<" "/utf8>> = ReadFun(<<"\\ ">>),
  <<"\""/utf8>> = ReadFun(<<"\\\"">>),

  ct:comment("Char EOF"),
  ok = try ReadAllFun(<<"12 \\">>)
       catch _:_ -> ok
       end,

  ct:comment("Octal char wrong length"),
  ok = try ReadAllFun(<<"12 \\o0337">>)
       catch _:_ -> ok
       end,

  ct:comment("Octal char wrong range"),
  ok = try ReadAllFun(<<"12 \\o477">>)
       catch _:_ -> ok
       end,

  ct:comment("Unsupported char"),
  ok = try ReadAllFun(<<"42.0 \\ab">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

fn(Config) ->
  ReadFun = ?config(read_fun, Config),

  FnSymbol = clj_rt:symbol(<<"fn*">>),
  EmptyVector = clj_rt:vector([]),
  EmptyList = clj_rt:list([]),

  ct:comment("Read empty anonymous fn"),
  EmptyFn = ReadFun(<<"#()">>),
  FnSymbol = clj_rt:first(EmptyFn),
  EmptyVector = clj_rt:second(EmptyFn),
  ?assertEquiv(clj_rt:third(EmptyFn), EmptyList),

  ct:comment("Read anonymous fn with %"),
  OneArgFn = ReadFun(<<"#(%)">>),
  FnSymbol = clj_rt:first(OneArgFn),
  ArgVector1 = clj_rt:second(OneArgFn),
  1 = clj_rt:count(ArgVector1),
  BodyList1 = clj_rt:third(OneArgFn),
  1 = clj_rt:count(BodyList1),
  true = clj_rt:first(ArgVector1) == clj_rt:first(ArgVector1),

  ct:comment("Read anonymous fn with %4 and %42"),
  FortyTwoArgFn = ReadFun(<<"#(do %42 %4)">>),
  FnSymbol = clj_rt:first(FortyTwoArgFn),
  ArgVector42 = clj_rt:second(FortyTwoArgFn),
  42 = clj_rt:count(ArgVector42),
  BodyList42 = clj_rt:third(FortyTwoArgFn),
  3 = clj_rt:count(BodyList42),
  Arg42Sym = clj_rt:second(BodyList42),
  {match, _} = re:run(clj_rt:name(Arg42Sym), "p42__\\d+#"),

  ct:comment("Read anonymous fn with %3 and %&"),
  RestArgFn = ReadFun(<<"#(do %& %3)">>),
  FnSymbol = clj_rt:first(RestArgFn),
  ArgVectorRest = clj_rt:second(RestArgFn),
  5 = clj_rt:count(ArgVectorRest), %% 1-3, & and rest
  BodyListRest = clj_rt:third(RestArgFn),
  3 = clj_rt:count(BodyListRest),
  ArgRestSym = clj_rt:second(BodyListRest),
  {match, _} = re:run(clj_rt:name(ArgRestSym), "rest__\\d+#"),

  ct:comment("Nested #()"),
  ok = try ReadFun(<<"#(do #(%))">>)
       catch error:Error1 ->
           Msg1 = 'clojerl.IError':message(Error1),
           <<?NO_SOURCE, ":1:7: Nested #()s are not allowed">> = Msg1,
           ok
       end,

  {comments, ""}.

arg(Config) ->
  ReadFun = ?config(read_fun, Config),
  ReadAllFun = ?config(read_all_fun, Config),

  ct:comment("Read % as a symbol"),
  ArgSymbol = clj_rt:symbol(<<"%">>),
  ?assertEquiv(ReadFun(<<"%">>), ArgSymbol),

  ct:comment("Read %1 as a symbol"),
  ArgOneSymbol = clj_rt:symbol(<<"%1">>),
  ?assertEquiv(ReadFun(<<"%1">>), ArgOneSymbol),

  erlang:put(arg_env, #{}),

  ct:comment("Read % as an argument"),
  ArgGenSymbol = ReadFun(<<"%">>),
  ArgGenName = clj_rt:name(ArgGenSymbol),
  {match, _} = re:run(ArgGenName, "p1__\\d+#"),

  ArgGenSymbol2 = ReadFun(<<"% ">>),
  ArgGenName2 = clj_rt:name(ArgGenSymbol2),
  {match, _} = re:run(ArgGenName2, "p1__\\d+#"),

  ct:comment("Read %1 as an argument"),
  ArgOneGenSymbol = ReadFun(<<"%1">>),
  ArgOneGenName = clj_rt:name(ArgOneGenSymbol),
  {match, _} = re:run(ArgOneGenName, "p1__\\d+#"),

  ct:comment("Read %42 as an argument"),
  ArgFortyTwoGenSymbol = ReadFun(<<"%42">>),
  ArgFortyTwoGenName = clj_rt:name(ArgFortyTwoGenSymbol),
  {match, _} = re:run(ArgFortyTwoGenName, "p42__\\d+#"),

  ct:comment("Read %& as an argument"),
  [ArgRestGenSymbol] = ReadAllFun(<<"%&">>),
  ArgRestGenName = clj_rt:name(ArgRestGenSymbol),
  {match, _} = re:run(ArgRestGenName, "rest__\\d+#"),

  ct:comment("Invalid char after %"),
  ok = try ReadFun(<<"%a">>)
       catch error:Error1 ->
           Msg1 = 'clojerl.IError':message(Error1),
           <<?NO_SOURCE, ":1:2: Arg literal must be %, %& or %integer">> = Msg1,
           ok
       end,

  erlang:erase(arg_env),

  {comments, ""}.

eval(Config) ->
  ReadFun = ?config(read_fun, Config),

  ct:comment("Read eval 1"),
  1 = ReadFun(<<"#=1">>),

  ct:comment("Read eval (do 1)"),
  1 = ReadFun(<<"#=(do 1)">>),

  ct:comment("Read eval (str 1)"),
  <<"1">> = ReadFun(<<"#=(clj_rt/str 1)">>),

  {comments, ""}.

var(Config) ->
  ReadFun = ?config(read_fun, Config),

  VarSymbol = clj_rt:symbol(<<"var">>),
  ListSymbol = clj_rt:symbol(<<"list">>),

  ct:comment(""),
  List = clj_rt:list([VarSymbol, ListSymbol]),
  ?assertEquiv(ReadFun(<<"#'list">>), List),

  {comments, ""}.

regex(Config) ->
  ReadFun = ?config(read_fun, Config),
  ReadAllFun = ?config(read_all_fun, Config),

  Regex1 = 'erlang.util.Regex':?CONSTRUCTOR(<<".?el\\.lo">>),
  Regex1 = ReadFun(<<"#\".?el\\.lo\"">>),

  Regex2 = 'erlang.util.Regex':?CONSTRUCTOR(<<"">>),
  Regex2 = ReadFun(<<"#\"\"">>),

  Regex3 = 'erlang.util.Regex':?CONSTRUCTOR(<<"foo\\\"">>),
  Regex3 = ReadFun(<<"#\"foo\\\"\"">>),

  ct:comment("EOF: unterminated regex"),
  ok = try ReadAllFun(<<"#\"a*">>)
       catch _:_ -> ok
       end,

  ct:comment("EOF: unterminated regex"),
  ok = try ReadAllFun(<<"#\"foo\\">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

unreadable_form(Config) ->
  ReadFun = ?config(read_fun, Config),

  ct:comment("Read unreadable"),
  ok = try ReadFun(<<"#<1>">>)
       catch error:Error1 ->
           Msg1 = 'clojerl.IError':message(Error1),
           <<?NO_SOURCE, ":1:1: Unreadable form">> = Msg1,
           ok
       end,

  {comments, ""}.

discard(Config) ->
  ReadFun = ?config(read_fun, Config),
  ReadAllFun = ?config(read_all_fun, Config),

  [1] = ReadAllFun(<<"#_ :hello 1">>),

  1 = ReadFun(<<"#_ :hello 1">>),

  1 = ReadFun(<<"1 #_ :hello">>),

  1 = ReadFun(<<"#_ #_ :hello :world 1">>),

  [1, 2] = clj_rt:to_list(ReadFun(<<"(1 2 #_ #_ #_ #_ 3 4 5 6)">>)),

  StrSym = clj_rt:symbol(<<"str">>),
  ByeKeyword = clj_rt:keyword(<<"bye">>),
  List = clj_rt:list([StrSym, ByeKeyword]),
  ?assertEquiv(ReadFun(<<"(str  #_ :hello :bye)">>), List),

  {comments, ""}.

'cond'(Config) when is_list(Config) ->
  ReadFun  = ?config(read_fun, Config),
  ReadFun2 = ?config(read_fun2, Config),

  AllowOpts = #{'read-cond' => allow},
  AllowCljFeatureOpts = #{'read-cond' => allow,
                          features => ReadFun(<<"#{:clj}">>)},
  AllowClrFeatureOpts = #{'read-cond' => allow,
                          features => ReadFun(<<"#{:clr}">>)},
  HelloKeyword = clj_rt:keyword(<<"hello">>),

  ct:comment("Allow with no features"),
  HelloKeyword = ReadFun2(<<"#?(:one 2) :hello">>, AllowOpts),

  ct:comment("Allow with feature match"),
  2 = ReadFun2(<<"#?(:clj 2) :hello">>, AllowCljFeatureOpts),

  ct:comment("Allow with no feature match"),
  HelloKeyword = ReadFun2( <<"#?(:clj 2 :cljs [3]) :hello">>
                         , AllowClrFeatureOpts
                         ),

  ct:comment("Cond splice vector"),
  OneTwoThreeVector = ReadFun(<<"[:one :two :three]">>),
  OneTwoThreeVector = ReadFun2( <<"[:one #?@(:clj :three"
                                  " :clr [:two]) :three]">>
                              , AllowClrFeatureOpts
                              ),

  OneTwoThreeVector = ReadFun2( <<"[:one #? @  (:clj :three"
                                  " :clr [:two]) :three]">>
                              , AllowClrFeatureOpts
                              ),

  OneTwoThreeFourVector = ReadFun(<<"[:one :two :three :four]">>),
  OneTwoThreeFourVector =
    ReadFun2( <<"[:one #?@(:clj :three :clr"
                " [:two :three] :cljs :five) :four]">>
            , AllowClrFeatureOpts
            ),

  ct:comment("Cond splice list"),
  OneTwoThreeVector = ReadFun2( <<"[:one #?@(:clj :three"
                                  " :clr (:two)) :three]">>
                              , AllowClrFeatureOpts
                              ),
  OneTwoThreeFourVector = ReadFun2( <<"[:one #?@(:clj :three :clr"
                                      " (:two :three) :cljs :five) :four]">>
                                  , AllowClrFeatureOpts
                                  ),

  ct:comment("Preserve read"),
  PreserveOpts = #{'read-cond' => preserve},
  ReaderCond   =
    'clojerl.reader.ReaderConditional':?CONSTRUCTOR(
                                          ReadFun(<<"(1 2)">>), false
                                         ),
  [ReaderCondCheck, HelloKeyword] =
    read_all(<<"#?(1 2) :hello">>, PreserveOpts),
  ?assertEquiv(ReaderCond, ReaderCondCheck),

  ReaderCondSplice =
    'clojerl.reader.ReaderConditional':?CONSTRUCTOR(
                                          ReadFun(<<"(1 2)">>), true
                                         ),
  ReaderCondSpliceVector = clj_rt:vector([ReaderCondSplice, HelloKeyword]),
  ReaderCondSpliceVectorCheck = ReadFun2(<<"[#?@(1 2) :hello]">>, PreserveOpts),
  ?assertEquiv(ReaderCondSpliceVector, ReaderCondSpliceVectorCheck),
  false = clj_rt:equiv(ReaderCond, ReaderCondSpliceVector),

  ct:comment("EOF while reading cond"),
  ok = try ReadFun2(<<"#?">>, AllowOpts)
       catch error:Error1 ->
           Msg1 = 'clojerl.IError':message(Error1),
           <<?NO_SOURCE, ":1:3: EOF while reading cond">> = Msg1,
           ok
       end,

  ct:comment("Reader conditional not allowed"),
  ok = try ReadFun(<<"#?(:clj :whatever :clr :whateverrrr)">>)
       catch error:Error2 ->
           Msg2 = 'clojerl.IError':message(Error2),
           <<?NO_SOURCE, ":1:3: Conditional read not allowed">> = Msg2,
           ok
       end,

  ct:comment("No list"),
  ok = try ReadFun2(<<"#?:clj">>, AllowOpts)
       catch error:Error3 ->
           Msg3 = 'clojerl.IError':message(Error3),
           <<?NO_SOURCE, ":1:3: read-cond body must be a list">> = Msg3,
           ok
       end,

  ct:comment("EOF: no feature matched"),
  ok = try ReadFun2(<<"#?(:clj :whatever :clr :whateverrrr)">>, AllowOpts)
       catch error:Error4 ->
           Msg4 = 'clojerl.IError':message(Error4),
           <<?NO_SOURCE, ":1:37: EOF">> = Msg4,
           ok
       end,

  ct:comment("Uneven number of forms"),
  ok = try ReadFun2(<<"#?(:one :two :three)">>, AllowOpts)
       catch error:Error5 ->
           Msg5 = 'clojerl.IError':message(Error5),
           <<?NO_SOURCE, ":1:21: read-cond requires an "
             "even number of forms">> = Msg5,
           ok
       end,

  ct:comment("Splice at the top level"),
  ok = try ReadFun2(<<"#?@(:clje [:two])">>, AllowOpts)
       catch error:Error6 ->
           Msg6 = 'clojerl.IError':message(Error6),
           <<?NO_SOURCE, ":1:5: Reader conditional splicing not allowed "
             "at the top level">> = Msg6,
           ok
       end,

  ct:comment("Splice in list but not sequential"),
  ok = try ReadFun2(<<"[#?@(:clr :a :cljs :b) :c :d]">>,
                           AllowClrFeatureOpts)
       catch error:Error7 ->
           Msg7 = 'clojerl.IError':message(Error7),
           <<?NO_SOURCE, ":1:13: Spliced form list in read-cond-splicing "
             "must extend clojerl.ISequential">> = Msg7,
           ok
       end,

  ct:comment("Feature is not a keyword"),
  ok = try ReadFun2(<<"#?(1 2) :hello">>, AllowOpts)
       catch error:Error8 ->
           Msg8 = 'clojerl.IError':message(Error8),
           <<?NO_SOURCE, ":1:4: Feature should be a keyword">> = Msg8,
           ok
       end,

  {comments, ""}.

unsupported_reader(Config) ->
  ReadFun  = ?config(read_fun, Config),
  ct:comment("Try unsupported reader"),
  ok = try ReadFun(<<"#-:something">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

erl_literals(Config) ->
  ReadFun  = ?config(read_fun, Config),

  %% Tuple
  ct:comment("Read an empty tuple"),
  {} = ReadFun(<<"#erl []">>),

  ct:comment("Read a tuple with a single element"),
  {42} = ReadFun(<<"#erl [42]">>),

  ct:comment("Read a tuple with many elements"),
  HelloKeyword = clj_rt:keyword(<<"hello">>),
  WorldSymbol = clj_rt:symbol(<<"world">>),
  { 42
  , HelloKeywordCheck
  , 2.5
  , WorldSymbolCheck
  } = ReadFun(<<"#erl [42, :hello, 2.5, world]">>),
  ?assertEquiv(HelloKeyword, HelloKeywordCheck),
  ?assertEquiv(WorldSymbol, WorldSymbolCheck),

  ct:comment("Read a tuple whose first element is an keyword"),
  T = ReadFun(<<"#erl [:random, :hello, 2.5, 'world]">>),
  'erlang.Tuple' = clj_rt:type_module(T),

  %% List
  ct:comment("Read an empty list"),
  ErlListSym         = clj_rt:symbol(<<"erl-list*">>),
  [ErlListSymCheck] = clj_rt:to_list(ReadFun(<<"#erl()">>)),
  ?assertEquiv(ErlListSym, ErlListSymCheck),

  ct:comment("Read a list with a single element"),
  [ErlListSymCheck, 42] = clj_rt:to_list(ReadFun(<<"#erl (42)">>)),

  ct:comment("Read a tuple with many elements"),
  [ ErlListSymCheck
  , 42
  , HelloKeywordCheckList
  , 2.5
  , WorldSymbolCheckList
  ] = clj_rt:to_list(ReadFun(<<"#erl (42, :hello, 2.5, world)">>)),
  ?assertEquiv(HelloKeyword, HelloKeywordCheckList),
  ?assertEquiv(WorldSymbol, WorldSymbolCheckList),

  %% Map
  ct:comment("Read an empty map"),
  #{} = ReadFun(<<"#erl {}">>),

  ct:comment("Read a map with a single entry"),
  #{42 := 'forty-two'} = ReadFun(<<"#erl {42 :forty-two}">>),

  ct:comment("Read a map with many entries"),
  #{ 42  := HelloKeywordCheckMap
   , 2.5 := WorldSymbolCheckMap
   } = ReadFun(<<"#erl {42, :hello, 2.5, world}">>),
  ?assertEquiv(HelloKeyword, HelloKeywordCheckMap),
  ?assertEquiv(WorldSymbol, WorldSymbolCheckMap),

  %% String
  ct:comment("Read a string"),
  [ErlListSymCheck] = clj_rt:to_list(ReadFun(<<"#erl\"\"">>)),

  ct:comment("Read a non-empty string"),
  [ ErlListSymCheck
  | "Hello there!"
  ] = clj_rt:to_list(ReadFun(<<"#erl \"Hello there!\"">>)),

  ct:comment("Try to read an integer"),
  ok = try ReadFun(<<"#erl 1">>), error
       catch _:_ -> ok
       end,

  {comments, ""}.

erl_binary(Config) ->
  ReadFun  = ?config(read_fun, Config),

  ErlBinarySym = clj_rt:symbol(<<"erl-binary*">>),
  EmptyBinary  = clj_rt:list([ErlBinarySym]),

  ct:comment("Read an empty binary"),
  ?assertEquiv(ReadFun(<<"#bin[]">>), EmptyBinary),

  ct:comment("Read a single integer"),
  SingleIntBinary = clj_rt:list([ErlBinarySym, 64]),
  ?assertEquiv(ReadFun(<<"#bin[64]">>), SingleIntBinary),

  {comments, ""}.

erl_alias(Config) ->
  ReadFun  = ?config(read_fun, Config),

  ErlAliasSym = clj_rt:symbol(<<"erl-alias*">>),
  XSym        = clj_rt:symbol(<<"x">>),
  AliasList   = clj_rt:list([ErlAliasSym, XSym, 1]),

  ct:comment("Read the tagged literal alias"),
  ?assertEquiv(ReadFun(<<"#as(x 1)">>), AliasList),

  {comments, ""}.

tagged(Config) ->
  ReadFun  = ?config(read_fun, Config),

  UUIDBin = <<"de305d54-75b4-431b-adb2-eb6b9e546014">>,

  ct:comment("Use bootstraped default readers implementation"),
  {inst, <<"2016">>} = ReadFun(<<"#inst \"2016\"">>),
  {uuid, UUIDBin} = ReadFun(<<"#uuid \"", UUIDBin/binary, "\"">>),

  ct:comment("Use *default-data-reader-fn*"),
  DefaultReaderFunVar =
    'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                              , <<"*default-data-reader-fn*">>
                              ),
  DefaultReaderFun = fun(_, _) -> default end,
  ok  = 'clojerl.Var':push_bindings(#{DefaultReaderFunVar => DefaultReaderFun}),
  default = ReadFun(<<"#whatever :tag">>),
  default = ReadFun(<<"#we use">>),
  ok  = 'clojerl.Var':pop_bindings(),

  ct:comment("Provide additional reader"),
  DataReadersVar = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                             , <<"*data-readers*">>
                                             ),
  BlaSymbol   = clj_rt:symbol(<<"bla">>),
  DataReaders = #{BlaSymbol => fun(_) -> bla end},

  ok  = 'clojerl.Var':push_bindings(#{DataReadersVar => DataReaders}),
  bla = ReadFun(<<"#bla 1">>),
  ok  = 'clojerl.Var':pop_bindings(),

  ct:comment("Don't provide a symbol"),
  ok = try ReadFun(<<"#1">>), error
       catch error:Error1 ->
           Msg1 = 'clojerl.IError':message(Error1),
           <<?NO_SOURCE, ":1:2: Reader tag must be a symbol">> = Msg1,
           ok
       end,

  ct:comment("Provide a missing reader"),
  ok = try ReadFun(<<"#bla 1">>), error
       catch error:Error2 ->
           Msg2 = 'clojerl.IError':message(Error2),
           <<?NO_SOURCE, ":1:2: No reader function for tag bla">> = Msg2,
           ok
       end,

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec read_io(binary()) -> any().
read_io(Src) ->
  read_io(Src, #{}).

-spec read_io(binary(), clj_reader:opts()) -> any().
read_io(Src, Opts) ->
  Reader         = 'erlang.io.StringReader':?CONSTRUCTOR(Src),
  PushbackReader = 'erlang.io.PushbackReader':?CONSTRUCTOR(Reader),
  clj_reader:read(<<>>, Opts#{?OPT_IO_READER => PushbackReader}).

-spec read(binary()) -> any().
read(Src) ->
  clj_reader:read(Src).

-spec read(binary(), clj_reader:opts()) -> any().
read(Src, Opts) ->
  clj_reader:read(Src, Opts).

-spec read_all_io(binary()) -> any().
read_all_io(Src) ->
  Reader         = 'erlang.io.StringReader':?CONSTRUCTOR(Src),
  PushbackReader = 'erlang.io.PushbackReader':?CONSTRUCTOR(Reader),
  read_all(<<>>, #{?OPT_IO_READER => PushbackReader}).

%% @doc Read all forms.
-spec read_all(binary()) -> [any()].
read_all(Src) ->
  read_all(Src, #{eof => ok}).

-spec read_all(binary(), clj_reader:opts()) -> [any()].
read_all(Src, Opts) ->
  Fun = fun(Form, Acc) -> [Form | Acc] end,
  lists:reverse(clj_reader:read_fold(Fun, Src, Opts, [])).
