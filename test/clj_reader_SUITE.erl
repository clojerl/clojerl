-module(clj_reader_SUITE).

-include("clojerl.hrl").

-export([all/0, init_per_suite/1]).

-export(
   [
    eof/1,
    number/1,
    string/1,
    keyword/1,
    symbol/1,
    comment/1,
    quote/1,
    deref/1,
    meta/1,
    syntax_quote/1,
    unquote/1,
    list/1,
    vector/1,
    map/1,
    set/1,
    unmatched_delim/1,
    char/1,
    arg/1,
    fn/1,
    eval/1,
    var/1,
    regex/1,
    unreadable_form/1,
    discard/1,
    'cond'/1,
    unsupported_reader/1,
    tuple/1,
    tagged/1
   ]
  ).

-type config() :: list().

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  clojerl:start(),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

eof(_Config) ->
  ct:comment("Read empty binary"),
  ok = try clj_reader:read(<<"">>)
       catch _:_ -> ok end,

  ok = try clj_reader:read(<<" , , , , ">>)
       catch _:_ -> ok end,

  ok = try clj_reader:read(<<", , , \t \n ,, ">>)
       catch _:_ -> ok end,

  {comments, ""}.

number(_Config) ->
  0   = clj_reader:read(<<"0">>),
  0.0 = clj_reader:read(<<"0.0">>),

  1   = clj_reader:read(<<"1">>),
  -1  = clj_reader:read(<<"-1">>),
  1.0 = clj_reader:read(<<"1.0">>),
  1.0 = clj_reader:read(<<"10E-1">>),
  1.0 = clj_reader:read(<<"10.0e-1">>),

  42 = clj_reader:read(<<"0x2A">>),
  42 = clj_reader:read(<<"052">>),
  42 = clj_reader:read(<<"36r16">>),
  42 = clj_reader:read(<<"36R16">>),

  {ratio, 1, 2} = clj_reader:read(<<"1/2">>),

  ok = try clj_reader:read(<<"12A">>)
       catch _:_ -> ok
       end,

  [0, 1, 2.0, 3.0e-10] = clj_reader:read_all(<<"0N 1 2.0 3e-10">>),

  {comments, ""}.

string(_Config) ->
  <<"hello">> = clj_reader:read(<<"\"hello\"">>),

  <<"hello \t world!">> = clj_reader:read(<<"\"hello \\t world!\"">>),
  <<"hello \n world!">> = clj_reader:read(<<"\"hello \\n world!\"">>),
  <<"hello \r world!">> = clj_reader:read(<<"\"hello \\r world!\"">>),
  <<"hello \" world!">> = clj_reader:read(<<"\"hello \\\" world!\"">>),
  <<"hello \f world!">> = clj_reader:read(<<"\"hello \\f world!\"">>),
  <<"hello \b world!">> = clj_reader:read(<<"\"hello \\b world!\"">>),
  <<"hello \\ world!">> = clj_reader:read(<<"\"hello \\\\ world!\"">>),

  <<"hello © world!"/utf8>> =
    clj_reader:read(<<"\"hello \\u00A9 world!\"">>),
  ok = try clj_reader:read(<<"\"hello \\u00A world!\"">>)
       catch _:_ -> ok
       end,
  ok = try clj_reader:read(<<"\"hello \\u00Z world!\"">>)
       catch _:_ -> ok
       end,

  <<"hello © world!"/utf8>> =
    clj_reader:read(<<"\"hello \\251 world!\"">>),

  ct:comment("EOF"),
  ok = try clj_reader:read(<<"\"hello world!">>)
       catch _:_ -> ok
       end,

  ct:comment("Octal not in range"),
  ok = try clj_reader:read(<<"\"hello \\400 world!">>)
       catch _:_ -> ok
       end,

  ct:comment("Number not in base"),
  ok = try clj_reader:read(<<"\"hello \\u000Z world!">>)
       catch _:_ -> ok
       end,

  ct:comment("Unsupported escaped char"),
  ok = try clj_reader:read(<<"\"hello \\z world!">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

keyword(_Config) ->
  SomeNsSymbol = clj_core:symbol(<<"some-ns">>),
  Env = clj_env:default(),
  clj_namespace:find_or_create(SomeNsSymbol),

  Keyword1 = clj_core:keyword(<<"hello-world">>),
  Keyword1 = clj_reader:read(<<":hello-world">>, #{}, Env),

  Keyword2 = clj_core:keyword(<<"some-ns">>, <<"hello-world">>),
  Keyword2 = clj_reader:read(<<"::hello-world">>, #{}, Env),

  Keyword3 = clj_core:keyword(<<"another-ns">>, <<"hello-world">>),
  Keyword3 = clj_reader:read(<<":another-ns/hello-world">>, #{}, Env),

  Keyword4 = clj_core:keyword(<<"/">>),
  Keyword4 = clj_reader:read(<<":/">>, #{}, Env),

  Keyword5 = clj_core:keyword(<<"some-ns">>, <<"/">>),
  Keyword5 = clj_reader:read(<<":some-ns//">>, #{}, Env),

  ct:comment("Error: triple colon :::"),
  ok = try clj_reader:read(<<":::hello-world">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: empty name after namespace"),
  ok = try clj_reader:read(<<":some-ns/">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: colon as last char"),
  ok = try clj_reader:read(<<":hello-world:">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: single colon"),
  ok = try clj_reader:read(<<":">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: numeric first char in name"),
  ok = try clj_reader:read(<<":42hello-world">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

symbol(_Config) ->
  Symbol1 = clj_core:symbol(<<"hello-world">>),
  true    = clj_core:equiv(clj_reader:read(<<"hello-world">>), Symbol1),

  Symbol2 = clj_core:symbol(<<"some-ns">>, <<"hello-world">>),
  true    = clj_core:equiv(clj_reader:read(<<"some-ns/hello-world">>), Symbol2),

  Symbol3 = clj_core:symbol(<<"another-ns">>, <<"hello-world">>),
  true    = clj_core:equiv( clj_reader:read(<<"another-ns/hello-world">>)
                          , Symbol3
                          ),

  Symbol4 = clj_core:symbol(<<"some-ns">>, <<"/">>),
  true    = clj_core:equiv(clj_reader:read(<<"some-ns//">>), Symbol4),

  ct:comment("nil, true & false"),
  undefined = clj_reader:read(<<"nil">>),
  true = clj_reader:read(<<"true">>),
  false = clj_reader:read(<<"false">>),

  ct:comment("Error: empty name after namespace"),
  ok = try clj_reader:read(<<"some-ns/">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: colon as last char"),
  ok = try clj_reader:read(<<"hello-world:">>)
       catch _:_ -> ok
       end,

  ct:comment("Error: numeric first char in name"),
  ok = try clj_reader:read(<<"42hello-world">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

comment(_Config) ->
  BlaKeyword = clj_core:keyword(<<"bla">>),

  ct:comment("Single semi-colon"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 ; comment\n :bla ">>),

  ct:comment("Two semi-colon"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 ;; comment\n :bla ">>),

  ct:comment("A bunch of semi-colons"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 ;;;; comment\n :bla ">>),

  ct:comment("Comment reader"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 #! comment\n :bla ">>),

  {comments, ""}.

quote(_Config) ->
  QuoteSymbol = clj_core:symbol(<<"quote">>),
  ListSymbol = clj_core:symbol(<<"list">>),

  ct:comment("Quote number"),
  ListQuote1 = clj_core:list([QuoteSymbol, 1]),
  ListQuote1 = clj_reader:read(<<"'1">>),

  ct:comment("Quote symbol"),
  ListQuote2 = clj_core:list([QuoteSymbol, ListSymbol]),
  true       = clj_core:equiv(clj_reader:read(<<"'list">>), ListQuote2),

  ct:comment("Quote space symbol"),
  true       = clj_core:equiv(clj_reader:read(<<"' list">>), ListQuote2),

  ct:comment("Error: only provide ' "),
  ok = try clj_reader:read(<<"'">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

deref(_Config) ->
  DerefSymbol = clj_core:symbol(<<"clojure.core">>, <<"deref">>),
  ListSymbol = clj_core:symbol(<<"list">>),

  ct:comment("Deref number :P"),
  ListDeref1 = clj_core:list([DerefSymbol, 1]),
  ListDeref1= clj_reader:read(<<"@1">>),

  ct:comment("Deref symbol :P"),
  ListDeref2 = clj_core:list([DerefSymbol, ListSymbol]),
  true       = clj_core:equiv(clj_reader:read(<<"@list">>), ListDeref2),

  ct:comment("Deref symbol :P and read other stuff"),
  [ListDeref3, 42.0] = clj_reader:read_all(<<"@list 42.0">>),
  true = clj_core:equiv(ListDeref3, ListDeref2),

  ct:comment("Error: only provide @ "),
  ok = try clj_reader:read(<<"@">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

meta(_Config) ->
  MetadataKw = clj_reader:read(<<"{:private true}">>),
  MetadataSym = clj_reader:read(<<"{:tag private}">>),

  ct:comment("Keyword doesn't support metadata"),
  ok = try clj_reader:read(<<"^:private :hello">>), error
       catch _:_ -> ok
       end,

  ct:comment("Keyword meta to symbol"),
  SymbolWithMetaKw = clj_reader:read(<<"^:private hello">>),
  true = clj_core:equiv(MetadataKw, clj_core:meta(SymbolWithMetaKw)),

  ct:comment("Symbol meta to symbol"),
  SymbolWithMetaSym = clj_reader:read(<<"^private hello">>),
  true = clj_core:equiv(clj_core:meta(SymbolWithMetaSym), MetadataSym),

  ct:comment("Map meta to symbol"),
  MapWithMetaKw = clj_reader:read(<<"^:private {}">>),
  true = clj_core:equiv(clj_core:meta(MapWithMetaKw), MetadataKw),

  ct:comment("Map meta to symbol"),
  MapWithMetaSym = clj_reader:read(<<"^private {}">>),
  true = clj_core:equiv(clj_core:meta(MapWithMetaSym), MetadataSym),

  ct:comment("List meta to symbol"),
  ListWithMetaKw = clj_reader:read(<<"^:private ()">>),
  true = clj_core:equiv(clj_core:meta(ListWithMetaKw), MetadataKw),

  ct:comment("List meta to symbol"),
  ListWithMetaSym = clj_reader:read(<<"^private ()">>),
  true = clj_core:equiv(clj_core:meta(ListWithMetaSym), MetadataSym),

  ct:comment("Vector meta to symbol"),
  VectorWithMetaKw = clj_reader:read(<<"^:private []">>),
  true = clj_core:equiv(clj_core:meta(VectorWithMetaKw), MetadataKw),

  ct:comment("Vector meta to symbol"),
  VectorWithMetaSym = clj_reader:read(<<"^private []">>),
  true = clj_core:equiv(clj_core:meta(VectorWithMetaSym), MetadataSym),

  ct:comment("Set meta to symbol"),
  SetWithMetaKw = clj_reader:read(<<"^:private #{}">>),
  true = clj_core:equiv(clj_core:meta(SetWithMetaKw), MetadataKw),

  ct:comment("Set meta to symbol"),
  SetWithMetaSym = clj_reader:read(<<"^private #{}">>),
  true = clj_core:equiv(clj_core:meta(SetWithMetaSym), MetadataSym),

  ct:comment("Meta number"),
  ok = try clj_reader:read(<<"^1 1">>)
       catch _:_ -> ok
       end,

  ct:comment("Reader meta number"),
  ok = try clj_reader:read(<<"#^1 1">>)
       catch _:_ -> ok
       end,

  ct:comment("Meta without form"),
  ok = try clj_reader:read(<<"^:private">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

syntax_quote(_Config) ->
  WithMetaSym = clj_core:symbol(<<"clojure.core">>, <<"with-meta">>),
  QuoteSym = clj_core:symbol(<<"quote">>),
  QuoteFun = fun(X) -> clj_core:list([QuoteSym, X]) end,

  ct:comment("Read special form"),
  DoSym = clj_core:symbol(<<"do">>),
  QuoteDoList = QuoteFun(DoSym),
  DoSyntaxQuote = clj_reader:read(<<"`do">>),
  true = clj_core:equiv(clj_core:first(DoSyntaxQuote), WithMetaSym),
  true = clj_core:equiv(clj_core:second(DoSyntaxQuote), QuoteDoList),

  DefSym = clj_core:symbol(<<"def">>),

  QuoteDefList = clj_core:list([QuoteSym, DefSym]),
  DefSyntaxQuote = clj_reader:read(<<"`def">>),
  true = clj_core:equiv(clj_core:first(DefSyntaxQuote), WithMetaSym),
  true = clj_core:equiv(clj_core:second(DefSyntaxQuote), QuoteDefList),

  ct:comment("Read literals"),
  1 = clj_reader:read(<<"`1">>),
  42.0 = clj_reader:read(<<"`42.0">>),
  <<"something!">> = clj_reader:read(<<"`\"something!\"">>),

  ct:comment("Keywords can't have metadata"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  HelloKeyword = clj_reader:read(<<"`:hello">>),

  ct:comment("Read values that can have metadata"),

  ct:comment("Read unqualified symbol"),
  UserHelloSym = clj_core:symbol(<<"clojure.core">>, <<"hello">>),
  HelloSyntaxQuote = clj_reader:read(<<"`hello">>),
  true = clj_core:equiv(clj_core:first(HelloSyntaxQuote), WithMetaSym),
  true = clj_core:equiv( clj_core:second(HelloSyntaxQuote)
                       , QuoteFun(UserHelloSym)
                       ),

  ct:comment("Read qualified symbol"),
  SomeNsHelloSym = clj_core:symbol(<<"some-ns">>, <<"hello">>),
  SomeNsHelloSyntaxQuote = clj_reader:read(<<"`some-ns/hello">>),
  true = clj_core:equiv(clj_core:first(SomeNsHelloSyntaxQuote), WithMetaSym),
  true = clj_core:equiv( clj_core:second(SomeNsHelloSyntaxQuote)
                       , QuoteFun(SomeNsHelloSym)
                       ),

  ct:comment("Read auto-gen symbol"),
  ListGenSym = clj_reader:read(<<"`hello#">>),
  QuotedGenSym = clj_core:second(ListGenSym),
  GenSymName = clj_core:name(clj_core:second(QuotedGenSym)),
  {match, _} = re:run(GenSymName, "hello__\\d+__auto__"),

  ct:comment("Read auto-gen symbol, "
             "check generated symbols have the same name"),
  ListGenSym2 = clj_reader:read(<<"`(hello# hello# world#)">>),
  ListApply  = clj_core:second(ListGenSym2),
  ListConcat = clj_core:third(ListApply),
  ListSecond = clj_core:second(ListConcat),
  ListThird  = clj_core:third(ListConcat),
  true = clj_core:equiv( clj_core:second(ListSecond)
                       , clj_core:second(ListThird)
                       ),

  ct:comment("Read unquote"),
  HelloSym = clj_core:symbol(<<"hello">>),
  ListWithMetaHelloSym = clj_reader:read(<<"`~hello">>),
  true = clj_core:equiv(clj_core:first(ListWithMetaHelloSym), WithMetaSym),
  true = clj_core:equiv(clj_core:second(ListWithMetaHelloSym), HelloSym),

  ct:comment("Use unquote splice not in list"),
  ok = try clj_reader:read(<<"`~@(hello)">>)
       catch _:_ -> ok end,

  ct:comment("Read list and empty list"),
  WithMetaListHello = clj_reader:read(<<"`(hello :world)">>),
  ListHelloCheck = clj_reader:read(<<"(clojure.core/concat"
                                     "  (clojure.core/list 'clojure.core/hello)"
                                     "  (clojure.core/list :world))">>),
  true = clj_core:equiv( clj_core:third(clj_core:second(WithMetaListHello))
                       , ListHelloCheck
                       ),

  WithMetaEmptyList = clj_reader:read(<<"`()">>),
  EmptyListCheck = clj_reader:read(<<"(clojure.core/list)">>),
  true = clj_core:equiv( clj_core:third(clj_core:second(WithMetaEmptyList))
                       , EmptyListCheck
                       ),

  ct:comment("Read map"),
  MapWithMeta = clj_reader:read(<<"`{hello :world}">>),
  MapWithMetaCheck =
    clj_reader:read(<<"(clojure.core/apply"
                      "  clojure.core/hash-map"
                      "  (clojure.core/concat"
                      "    (clojure.core/list 'clojure.core/hello)"
                      "    (clojure.core/list :world)))">>),
  true = clj_core:equiv(clj_core:first(MapWithMeta), WithMetaSym),
  true = clj_core:equiv(clj_core:second(MapWithMeta), MapWithMetaCheck),

  ct:comment("Read vector"),
  VectorWithMeta = clj_reader:read(<<"`[hello :world]">>),
  VectorWithMetaCheck =
    clj_reader:read(<<"(clojure.core/apply"
                      "  clojure.core/vector"
                      "  (clojure.core/concat"
                      "    (clojure.core/list 'clojure.core/hello)"
                      "    (clojure.core/list :world)))">>),
  true = clj_core:equiv(clj_core:first(VectorWithMeta), WithMetaSym),
  true = clj_core:equiv(clj_core:second(VectorWithMeta), VectorWithMetaCheck),

  ct:comment("Read set"),
  SetWithMeta = clj_reader:read(<<"`#{hello :world}">>),
  SetWithMetaCheck =
    clj_reader:read(<<"(clojure.core/apply"
                      "  clojure.core/hash-set"
                      "  (clojure.core/concat"
                      "    (clojure.core/list :world)"
                      "    (clojure.core/list 'clojure.core/hello)))">>),
  true = clj_core:equiv(clj_core:first(SetWithMeta), WithMetaSym),
  true = clj_core:equiv(clj_core:second(SetWithMeta), SetWithMetaCheck),

  ct:comment("Read unquote-splice inside list"),
  WithMetaHelloWorldSup = clj_reader:read(<<"`(~@(hello world) :sup?)">>),
  HelloWorldSupCheck = clj_reader:read(<<"(clojure.core/concat"
                                         "  (hello world)"
                                         "  (clojure.core/list :sup?))">>),
  true = clj_core:equiv( clj_core:third(clj_core:second(WithMetaHelloWorldSup))
                       , HelloWorldSupCheck
                       ),

  ct:comment("Read unquote inside list"),
  ListWithMetaHelloWorld = clj_reader:read(<<"`(~hello :world)">>),
  ListHelloWorldCheck =
    clj_reader:read(<<"(clojure.core/concat"
                      "  (clojure.core/list hello)"
                      "  (clojure.core/list :world))">>),
  true = clj_core:equiv( clj_core:third(clj_core:second(ListWithMetaHelloWorld))
                       , ListHelloWorldCheck
                       ),

  {comments, ""}.

unquote(_Config) ->
  UnquoteSymbol = clj_core:symbol(<<"clojure.core">>, <<"unquote">>),
  UnquoteSplicingSymbol = clj_core:symbol(<<"clojure.core">>,
                                               <<"unquote-splicing">>),
  HelloWorldSymbol = clj_core:symbol(<<"hello-world">>),

  ct:comment("Unquote"),
  ListUnquote = clj_core:list([UnquoteSymbol, HelloWorldSymbol]),
  true = clj_core:equiv(clj_reader:read(<<"~hello-world">>), ListUnquote),

  ct:comment("Unquote splicing"),
  ListUnquoteSplicing = clj_core:list([UnquoteSplicingSymbol,
                                            HelloWorldSymbol]),
  true =
    clj_core:equiv(clj_reader:read(<<"~@hello-world">>), ListUnquoteSplicing),

  ct:comment("Unquote nothing"),
  ok = try clj_reader:read(<<"~">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

list(_Config) ->
  HelloWorldKeyword = clj_core:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_core:symbol(<<"hello-world">>),

  ct:comment("Empty List"),
  EmptyList = clj_core:list([]),
  true = clj_core:equiv(clj_reader:read(<<"()">>), EmptyList),

  ct:comment("List"),
  List = clj_core:list([HelloWorldKeyword, HelloWorldSymbol]),
  true = clj_core:equiv( clj_reader:read(<<"(:hello-world hello-world)">>)
                       , List
                       ),

  ct:comment("List & space"),
  true = clj_core:equiv( clj_reader:read(<<"(:hello-world hello-world )">>)
                       , List
                       ),

  ct:comment("List without closing paren"),
  ok = try clj_reader:read(<<"(1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

vector(_Config) ->
  HelloWorldKeyword = clj_core:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_core:symbol(<<"hello-world">>),

  ct:comment("Vector"),
  Vector = 'clojerl.Vector':?CONSTRUCTOR([HelloWorldKeyword, HelloWorldSymbol]),
  true = clj_core:equiv( clj_reader:read(<<"[:hello-world hello-world]">>)
                       , Vector
                       ),

  ct:comment("Vector without closing bracket"),
  ok = try clj_reader:read(<<"[1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

map(_Config) ->
  HelloWorldKeyword = clj_core:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_core:symbol(<<"hello-world">>),

  ct:comment("Map"),
  Map = 'clojerl.Map':?CONSTRUCTOR([ HelloWorldKeyword
                                   , HelloWorldSymbol
                                   , HelloWorldSymbol
                                   , HelloWorldKeyword
                                   ]
                                  ),
  MapResult = clj_reader:read(<<"{:hello-world hello-world,"
                                " hello-world :hello-world}">>),
  true = clj_core:equiv(Map, MapResult),

  ct:comment("Map without closing braces"),
  ok = try clj_reader:read(<<"{1 42.0">>)
       catch _:_ -> ok
       end,

  ct:comment("Literal map with odd number of expressions"),
  ok = try clj_reader:read(<<"{1 42.0 :a}">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

set(_Config) ->
  HelloWorldKeyword = clj_core:keyword(<<"hello-world">>),
  HelloWorldSymbol = clj_core:symbol(<<"hello-world">>),

  ct:comment("Set"),
  Set = 'clojerl.Set':?CONSTRUCTOR([HelloWorldKeyword, HelloWorldSymbol]),
  SetResult = clj_reader:read(<<"#{:hello-world hello-world}">>),
  true = clj_core:equiv(Set, SetResult),

  ct:comment("Set without closing braces"),
  ok = try clj_reader:read(<<"#{1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

unmatched_delim(_Config) ->
  ct:comment("Single closing paren"),
  ok = try clj_reader:read_all(<<"{1 42.0} )">>)
       catch _:_ -> ok
       end,

  ct:comment("Single closing bracket"),
  ok = try clj_reader:read_all(<<"{1 42.0} ]">>)
       catch _:_ -> ok
       end,

  ct:comment("Single closing braces"),
  ok = try clj_reader:read_all(<<"{1 42.0} } ">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

char(_Config) ->
  ct:comment("Read single char"),
  <<"a">> = clj_reader:read(<<"\\a">>),

  <<"\n">> = clj_reader:read(<<"\\newline">>),
  <<" ">> = clj_reader:read(<<"\\space">>),
  <<"\t">> = clj_reader:read(<<"\\tab">>),
  <<"\b">> = clj_reader:read(<<"\\backspace">>),
  <<"\f">> = clj_reader:read(<<"\\formfeed">>),
  <<"\r">> = clj_reader:read(<<"\\return">>),

  <<"©"/utf8>> = clj_reader:read(<<"\\u00A9">>),
  <<"ß"/utf8>> = clj_reader:read(<<"\\o337">>),

  <<" "/utf8>> = clj_reader:read(<<"\\ ">>),
  <<"\""/utf8>> = clj_reader:read(<<"\\\"">>),

  ct:comment("Char EOF"),
  ok = try clj_reader:read_all(<<"12 \\">>)
       catch _:_ -> ok
       end,

  ct:comment("Octal char wrong length"),
  ok = try clj_reader:read_all(<<"12 \\o0337">>)
       catch _:_ -> ok
       end,

  ct:comment("Octal char wrong range"),
  ok = try clj_reader:read_all(<<"12 \\o477">>)
       catch _:_ -> ok
       end,

  ct:comment("Unsupported char"),
  ok = try clj_reader:read_all(<<"42.0 \\ab">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

fn(_Config) ->
  FnSymbol = clj_core:symbol(<<"fn*">>),
  EmptyVector = clj_core:vector([]),
  EmptyList = clj_core:list([]),

  ct:comment("Read empty anonymous fn"),
  EmptyFn = clj_reader:read(<<"#()">>),
  FnSymbol = clj_core:first(EmptyFn),
  EmptyVector = clj_core:second(EmptyFn),
  true = clj_core:equiv(clj_core:third(EmptyFn), EmptyList),

  ct:comment("Read anonymous fn with %"),
  OneArgFn = clj_reader:read(<<"#(%)">>),
  FnSymbol = clj_core:first(OneArgFn),
  ArgVector1 = clj_core:second(OneArgFn),
  1 = clj_core:count(ArgVector1),
  BodyList1 = clj_core:third(OneArgFn),
  1 = clj_core:count(BodyList1),
  true = clj_core:first(ArgVector1) == clj_core:first(ArgVector1),

  ct:comment("Read anonymous fn with %4 and %42"),
  FortyTwoArgFn = clj_reader:read(<<"#(do %42 %4)">>),
  FnSymbol = clj_core:first(FortyTwoArgFn),
  ArgVector42 = clj_core:second(FortyTwoArgFn),
  42 = clj_core:count(ArgVector42),
  BodyList42 = clj_core:third(FortyTwoArgFn),
  3 = clj_core:count(BodyList42),
  Arg42Sym = clj_core:second(BodyList42),
  {match, _} = re:run(clj_core:name(Arg42Sym), "p42__\\d+#"),

  ct:comment("Read anonymous fn with %3 and %&"),
  RestArgFn = clj_reader:read(<<"#(do %& %3)">>),
  FnSymbol = clj_core:first(RestArgFn),
  ArgVectorRest = clj_core:second(RestArgFn),
  5 = clj_core:count(ArgVectorRest), %% 1-3, & and rest
  BodyListRest = clj_core:third(RestArgFn),
  3 = clj_core:count(BodyListRest),
  ArgRestSym = clj_core:second(BodyListRest),
  {match, _} = re:run(clj_core:name(ArgRestSym), "rest__\\d+#"),

  ct:comment("Nested #()"),
  ok = try clj_reader:read(<<"#(do #(%))">>)
       catch _:<<"?:1:7: Nested #()s are not allowed">> -> ok end,

  {comments, ""}.

arg(_Config) ->
  ct:comment("Read % as a symbol"),
  ArgSymbol = clj_core:symbol(<<"%">>),
  true = clj_core:equiv(clj_reader:read(<<"%">>), ArgSymbol),

  ct:comment("Read %1 as a symbol"),
  ArgOneSymbol = clj_core:symbol(<<"%1">>),
  true = clj_core:equiv(clj_reader:read(<<"%1">>), ArgOneSymbol),

  erlang:put(arg_env, #{}),

  ct:comment("Read % as an argument"),
  ArgGenSymbol = clj_reader:read(<<"%">>),
  ArgGenName = clj_core:name(ArgGenSymbol),
  {match, _} = re:run(ArgGenName, "p1__\\d+#"),

  ArgGenSymbol2 = clj_reader:read(<<"% ">>),
  ArgGenName2 = clj_core:name(ArgGenSymbol2),
  {match, _} = re:run(ArgGenName2, "p1__\\d+#"),

  ct:comment("Read %1 as an argument"),
  ArgOneGenSymbol = clj_reader:read(<<"%1">>),
  ArgOneGenName = clj_core:name(ArgOneGenSymbol),
  {match, _} = re:run(ArgOneGenName, "p1__\\d+#"),

  ct:comment("Read %42 as an argument"),
  ArgFortyTwoGenSymbol = clj_reader:read(<<"%42">>),
  ArgFortyTwoGenName = clj_core:name(ArgFortyTwoGenSymbol),
  {match, _} = re:run(ArgFortyTwoGenName, "p42__\\d+#"),

  ct:comment("Read %& as an argument"),
  [ArgRestGenSymbol] = clj_reader:read_all(<<"%&">>),
  ArgRestGenName = clj_core:name(ArgRestGenSymbol),
  {match, _} = re:run(ArgRestGenName, "rest__\\d+#"),

  ct:comment("Invalid char after %"),
  ok = try clj_reader:read(<<"%a">>)
       catch _:<<"?:1:1: Arg literal must be %, %& or %integer">> -> ok
       end,

  erlang:erase(arg_env),

  {comments, ""}.

eval(_Config) ->
  ct:comment("Read eval 1"),
  1 = clj_reader:read(<<"#=1">>),

  ct:comment("Read eval (do 1)"),
  1 = clj_reader:read(<<"#=(do 1)">>),

  ct:comment("Read eval (str 1)"),
  <<"1">> = clj_reader:read(<<"#=(clj_core/str.e 1)">>),

  {comments, ""}.

var(_Config) ->
  VarSymbol = clj_core:symbol(<<"var">>),
  ListSymbol = clj_core:symbol(<<"list">>),

  ct:comment(""),
  List = clj_core:list([VarSymbol, ListSymbol]),
  true = clj_core:equiv(clj_reader:read(<<"#'list">>), List),

  {comments, ""}.

regex(_Config) ->
  Regex = 'erlang.util.Regex':?CONSTRUCTOR(<<".?el\\.lo">>),
  Regex = clj_reader:read(<<"#\".?el\\.lo\"">>),

  ct:comment("EOF: unterminated regex"),
  ok = try clj_reader:read_all(<<"#\"a*">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

unreadable_form(_Config) ->
  ct:comment("Read unreadable"),
  ok = try clj_reader:read(<<"#<1>">>)
       catch _:<<"?:1:1: Unreadable form">> -> ok
       end,

  {comments, ""}.

discard(_Config) ->
  [1] = clj_reader:read_all(<<"#_ :hello 1">>),

  1 = clj_reader:read(<<"#_ :hello 1">>),

  1 = clj_reader:read(<<"1 #_ :hello">>),

  StrSym = clj_core:symbol(<<"str">>),
  ByeKeyword = clj_core:keyword(<<"bye">>),
  List = clj_core:list([StrSym, ByeKeyword]),
  true = clj_core:equiv(clj_reader:read(<<"(str  #_ :hello :bye)">>), List),

  {comments, ""}.

'cond'(_Config) ->
  AllowOpts = #{read_cond => allow},
  AllowCljFeatureOpts = #{read_cond => allow,
                          features => clj_reader:read(<<"#{:clj}">>)},
  AllowClrFeatureOpts = #{read_cond => allow,
                          features => clj_reader:read(<<"#{:clr}">>)},
  HelloKeyword = clj_core:keyword(<<"hello">>),

  ct:comment("Allow with no features"),
  HelloKeyword = clj_reader:read(<<"#?(1 2) :hello">>, AllowOpts),

  ct:comment("Allow with feature match"),
  2 = clj_reader:read(<<"#?(:clj 2) :hello">>, AllowCljFeatureOpts),

  ct:comment("Allow with no feature match"),
  HelloKeyword = clj_reader:read(<<"#?(:clj 2 :cljs [3]) :hello">>,
                                 AllowClrFeatureOpts),

  ct:comment("Cond splice vector"),
  OneTwoThreeVector = clj_reader:read(<<"[:one :two :three]">>),
  OneTwoThreeVector = clj_reader:read(<<"[:one #?@(:clj :three"
                                        " :clr [:two]) :three]">>,
                                      AllowClrFeatureOpts),

  OneTwoThreeFourVector = clj_reader:read(<<"[:one :two :three :four]">>),
  OneTwoThreeFourVector =
    clj_reader:read(<<"[:one #?@(:clj :three :clr"
                      " [:two :three] :cljs :five) :four]">>,
                    AllowClrFeatureOpts),

  ct:comment("Cond splice list"),
  OneTwoThreeVector = clj_reader:read(<<"[:one #?@(:clj :three"
                                        " :clr (:two)) :three]">>,
                                      AllowClrFeatureOpts),
  OneTwoThreeFourVector =
    clj_reader:read(<<"[:one #?@(:clj :three :clr"
                     " (:two :three) :cljs :five) :four]">>,
                    AllowClrFeatureOpts),

  ct:comment("Preserve read"),
  PreserveOpts = #{read_cond => preserve},
  ReaderCond   =
    'clojerl.reader.ReaderConditional':?CONSTRUCTOR(
                                          clj_reader:read(<<"(1 2)">>), false
                                         ),
  [ReaderCondCheck, HelloKeyword] =
    clj_reader:read_all(<<"#?(1 2) :hello">>, PreserveOpts),
  true = clj_core:equiv(ReaderCond, ReaderCondCheck),

  ReaderCondSplice =
    'clojerl.reader.ReaderConditional':?CONSTRUCTOR(
                                          clj_reader:read(<<"(1 2)">>), true
                                         ),
  ReaderCondSpliceVector = clj_core:vector([ReaderCondSplice, HelloKeyword]),
  ReaderCondSpliceVectorCheck =
    clj_reader:read(<<"[#?@(1 2) :hello]">>, PreserveOpts),
  true = clj_core:equiv(ReaderCondSpliceVector, ReaderCondSpliceVectorCheck),
  false = clj_core:equiv(ReaderCond, ReaderCondSpliceVector),

  ct:comment("EOF while reading cond"),
  ok = try clj_reader:read(<<"#?">>, AllowOpts)
       catch _:<<"?:1:3: EOF while reading cond">> -> ok
       end,

  ct:comment("Reader conditional not allowed"),
  ok = try clj_reader:read(<<"#?(:clj :whatever :clr :whateverrrr)">>)
       catch _:<<"?:1:3: Conditional read not allowed">> -> ok
       end,

  ct:comment("No list"),
  ok = try clj_reader:read(<<"#?:clj">>, AllowOpts)
       catch _:<<"?:1:3: read-cond body must be a list">> -> ok
       end,

  ct:comment("EOF: no feature matched"),
  ok = try clj_reader:read(<<"#?(:clj :whatever :clr :whateverrrr)">>,
                           AllowOpts)
       catch _:<<"?:1:37: EOF">> -> ok
       end,

  ct:comment("Uneven number of forms"),
  ok = try clj_reader:read(<<"#?(:one :two :three)">>, AllowOpts)
       catch _:<<"?:1:21: read-cond requires an even number of forms">> -> ok
       end,

  ct:comment("Splice not in list"),
  ok = try clj_reader:read(<<"#?@(:one [:two])">>, AllowOpts)
       catch _:<<"?:1:3: cond-splice not in list">> -> ok
       end,

  ct:comment("Splice in list but not sequential"),
  ok = try clj_reader:read(<<"[#?@(:clr :a :cljs :b) :c :d]">>,
                           AllowClrFeatureOpts)
       catch _:<<"?:1:23: Spliced form list in read-cond-splicing must "
                  "extend clojerl.ISequential">> -> ok
       end,

  {comments, ""}.

unsupported_reader(_Config) ->
  ct:comment("Try unsupported reader"),
  ok = try clj_reader:read(<<"#-:something">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

tuple(_Config) ->
  ct:comment("Read an empty tuple"),
  {} = clj_reader:read(<<"#[]">>),

  ct:comment("Read a tuple with a single element"),
  {42} = clj_reader:read(<<"#[42]">>),

  ct:comment("Read a tuple with many elements"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  WorldSymbol = clj_core:symbol(<<"world">>),
  { 42
  , HelloKeywordCheck
  , 2.5
  , WorldSymbolCheck
  } = clj_reader:read(<<"#[42, :hello, 2.5, world]">>),
  true = clj_core:equiv(HelloKeyword, HelloKeywordCheck),
  true = clj_core:equiv(WorldSymbol, WorldSymbolCheck),

  ct:comment("Read a tuple whose first element is an keyword"),
  T = clj_reader:read(<<"#[:random, :hello, 2.5, 'world]">>),
  'clojerl.erlang.Tuple' = clj_core:type(T),

  {comments, ""}.

tagged(_Config) ->
  DefaultDataReadersVar = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                                    , <<"default-data-readers">>
                                                    ),

  IdFun              = fun(X) -> X end,
  DefaultDataReaders = clj_core:hash_map(
                         [ clj_core:symbol(<<"inst">>), IdFun
                         , clj_core:symbol(<<"uuid">>), IdFun
                         ]
                        ),

  meck:new('clojure.core', [passthrough]),
  meck:expect( 'clojure.core'
             , 'clojerl.Var':val_function(DefaultDataReadersVar)
             , fun() -> DefaultDataReaders end
             ),

  ct:comment("Use default readers"),
  <<"2016">> = clj_reader:read(<<"#inst \"2016\"">>),
  <<"de305d54-75b4-431b-adb2-eb6b9e546014">> =
    clj_reader:read(<<"#uuid \"de305d54-75b4-431b-adb2-eb6b9e546014\"">>),

  ct:comment("Use *default-data-reader-fn*"),
  DefaultReaderFunVar =
    'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                              , <<"*default-data-reader-fn*">>
                              ),
  DefaultReaderFun = fun(_) -> default end,
  ok  = 'clojerl.Var':push_bindings(#{DefaultReaderFunVar => DefaultReaderFun}),
  default = clj_reader:read(<<"#whatever :tag">>),
  default = clj_reader:read(<<"#we use">>),
  ok  = 'clojerl.Var':pop_bindings(),

  ct:comment("Provide additional reader"),
  DataReadersVar = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                             , <<"*data-readers*">>
                                             ),
  DataReaders    = clj_core:hash_map([ clj_core:symbol(<<"bla">>)
                                     , fun(_) -> bla end
                                     ]
                                    ),

  ok  = 'clojerl.Var':push_bindings(#{DataReadersVar => DataReaders}),
  bla = clj_reader:read(<<"#bla 1">>),
  ok  = 'clojerl.Var':pop_bindings(),

  ct:comment("Don't provide a symbol"),
  ok = try clj_reader:read(<<"#1">>), error
       catch _:<<"?:1:2: Reader tag must be a symbol">> -> ok end,

  ct:comment("Provide a missing reader"),
  ok = try clj_reader:read(<<"#bla 1">>), error
       catch _:<<"?:1:2: No reader function for tag bla">> -> ok end,

  meck:unload('clojure.core'),

  {comments, ""}.
