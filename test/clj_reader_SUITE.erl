-module(clj_reader_SUITE).

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
    erl_fun/1,
    unsupported_reader/1
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
  application:ensure_all_started(clojerl),
  Config.

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
  0 = clj_reader:read(<<"0">>),
  0.0 = clj_reader:read(<<"0.0">>),

  1 = clj_reader:read(<<"1">>),
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
  SomeNsSymbol = 'clojerl.Symbol':new(<<"some-ns">>),
  {_Ns, Env} = clj_env:find_or_create_ns(clj_env:default(), SomeNsSymbol),

  Keyword1 = 'clojerl.Keyword':new(<<"hello-world">>),
  Keyword1 = clj_reader:read(<<":hello-world">>, #{}, Env),

  Keyword2 = 'clojerl.Keyword':new(<<"some-ns">>, <<"hello-world">>),
  Keyword2 = clj_reader:read(<<"::hello-world">>, #{}, Env),

  Keyword3 = 'clojerl.Keyword':new(<<"another-ns">>, <<"hello-world">>),
  Keyword3 = clj_reader:read(<<":another-ns/hello-world">>, #{}, Env),

  Keyword4 = 'clojerl.Keyword':new(<<"/">>),
  Keyword4 = clj_reader:read(<<":/">>, #{}, Env),

  Keyword5 = 'clojerl.Keyword':new(<<"some-ns">>, <<"/">>),
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
  Symbol1 = 'clojerl.Symbol':new(<<"hello-world">>),
  Symbol1 = clj_reader:read(<<"hello-world">>),

  Symbol2 = 'clojerl.Symbol':new(<<"some-ns">>, <<"hello-world">>),
  Symbol2 = clj_reader:read(<<"some-ns/hello-world">>),

  Symbol3 = 'clojerl.Symbol':new(<<"another-ns">>, <<"hello-world">>),
  Symbol3 = clj_reader:read(<<"another-ns/hello-world">>),

  Symbol4 = 'clojerl.Symbol':new(<<"some-ns">>, <<"/">>),
  Symbol4 = clj_reader:read(<<"some-ns//">>),

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
  BlaKeyword = 'clojerl.Keyword':new(<<"bla">>),

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
  QuoteSymbol = 'clojerl.Symbol':new(<<"quote">>),
  ListSymbol = 'clojerl.Symbol':new(<<"list">>),

  ct:comment("Quote number"),
  ListQuote1 = 'clojerl.List':new([QuoteSymbol, 1]),
  ListQuote1 = clj_reader:read(<<"'1">>),

  ct:comment("Quote symbol"),
  ListQuote2 = 'clojerl.List':new([QuoteSymbol, ListSymbol]),
  ListQuote2 = clj_reader:read(<<"'list">>),

  ct:comment("Quote space symbol"),
  ListQuote2 = clj_reader:read(<<"' list">>),

  ct:comment("Error: only provide ' "),
  ok = try clj_reader:read(<<"'">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

deref(_Config) ->
  DerefSymbol = 'clojerl.Symbol':new(<<"deref">>),
  ListSymbol = 'clojerl.Symbol':new(<<"list">>),

  ct:comment("Deref number :P"),
  ListDeref1 = 'clojerl.List':new([DerefSymbol, 1]),
  ListDeref1= clj_reader:read(<<"@1">>),

  ct:comment("Deref symbol :P"),
  ListDeref2 = 'clojerl.List':new([DerefSymbol, ListSymbol]),
  ListDeref2 = clj_reader:read(<<"@list">>),

  ct:comment("Deref symbol :P and read other stuff"),
  [ListDeref2, 42.0] = clj_reader:read_all(<<"@list 42.0">>),

  ct:comment("Error: only provide @ "),
  ok = try clj_reader:read(<<"@">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

meta(_Config) ->
  MetadataKw = clj_reader:read(<<"{:private true}">>),
  MetadataSym = clj_reader:read(<<"{:tag private}">>),

  ct:comment("Keyword meta to keyword"),
  HelloWithMetaKw = clj_reader:read(<<"^:private :hello">>),
  MetadataKw = clj_core:meta(HelloWithMetaKw),

  ct:comment("Symbol meta to keyword"),
  HelloWithMetaSym = clj_reader:read(<<"^private :hello">>),
  MetadataSym = clj_core:meta(HelloWithMetaSym),

  ct:comment("Keyword meta to symbol"),
  SymbolWithMetaKw = clj_reader:read(<<"^:private hello">>),
  MetadataKw = clj_core:meta(SymbolWithMetaKw),

  ct:comment("Symbol meta to symbol"),
  SymbolWithMetaSym = clj_reader:read(<<"^private hello">>),
  MetadataSym = clj_core:meta(SymbolWithMetaSym),

  ct:comment("Map meta to symbol"),
  MapWithMetaKw = clj_reader:read(<<"^:private {}">>),
  MetadataKw = clj_core:meta(MapWithMetaKw),

  ct:comment("Map meta to symbol"),
  MapWithMetaSym = clj_reader:read(<<"^private {}">>),
  MetadataSym = clj_core:meta(MapWithMetaSym),

  ct:comment("List meta to symbol"),
  ListWithMetaKw = clj_reader:read(<<"^:private ()">>),
  MetadataKw = clj_core:meta(ListWithMetaKw),

  ct:comment("List meta to symbol"),
  ListWithMetaSym = clj_reader:read(<<"^private ()">>),
  MetadataSym = clj_core:meta(ListWithMetaSym),

  ct:comment("Vector meta to symbol"),
  VectorWithMetaKw = clj_reader:read(<<"^:private []">>),
  MetadataKw = clj_core:meta(VectorWithMetaKw),

  ct:comment("Vector meta to symbol"),
  VectorWithMetaSym = clj_reader:read(<<"^private []">>),
  MetadataSym = clj_core:meta(VectorWithMetaSym),

  ct:comment("Set meta to symbol"),
  SetWithMetaKw = clj_reader:read(<<"^:private #{}">>),
  MetadataKw = clj_core:meta(SetWithMetaKw),

  ct:comment("Set meta to symbol"),
  SetWithMetaSym = clj_reader:read(<<"^private #{}">>),
  MetadataSym = clj_core:meta(SetWithMetaSym),

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

  WrapWithMetaFun = fun(Value, Meta) ->
                         clj_core:list([WithMetaSym, Value, Meta])
                    end,

  ct:comment("Read special form"),
  DoSym = clj_core:symbol(<<"do">>),
  WithMetaDoSym = WrapWithMetaFun(clj_core:list([QuoteSym, DoSym]), undefined),
  WithMetaDoSym = clj_reader:read(<<"`do">>),

  DefSym = clj_core:symbol(<<"def">>),
  WithMetaDefSym =
    WrapWithMetaFun(clj_core:list([QuoteSym, DefSym]), undefined),
  WithMetaDefSym = clj_reader:read(<<"`def">>),

  ct:comment("Read literals"),
  1 = clj_reader:read(<<"`1">>),
  42.0 = clj_reader:read(<<"`42.0">>),
  <<"something!">> = clj_reader:read(<<"`\"something!\"">>),

  ct:comment("Read values that can have metadata"),
  HelloKeyword = clj_core:keyword(<<"hello">>),
  ListWithMetaHelloKw = WrapWithMetaFun(HelloKeyword, undefined),
  ListWithMetaHelloKw = clj_reader:read(<<"`:hello">>),

  ct:comment("Read unqualified symbol"),
  UserHelloSym = clj_core:symbol(<<"user">>, <<"hello">>),
  ListWithMetaUserHelloSym = WrapWithMetaFun(UserHelloSym, undefined),
  ListWithMetaUserHelloSym = clj_reader:read(<<"`hello">>),

  ct:comment("Read qualified symbol"),
  SomeNsHelloSym = clj_core:symbol(<<"some-ns">>, <<"hello">>),
  ListWithMetaSomeNsHelloSym = WrapWithMetaFun(SomeNsHelloSym, undefined),
  ListWithMetaSomeNsHelloSym = clj_reader:read(<<"`some-ns/hello">>),

  ListWithMetaUserHelloSym = clj_reader:read(<<"`user/hello">>),

  ct:comment("Read auto-gen symbol"),
  ListGenSym = clj_reader:read(<<"`hello#">>),
  GenSym = clj_core:second(ListGenSym),
  GenSymName = clj_core:name(GenSym),
  {match, _} = re:run(GenSymName, "hello__\\d+__auto__"),

  ct:comment("Read auto-gen symbol, "
             "check generated symbols have the same name"),
  ListGenSym2 = clj_reader:read(<<"`(hello# hello# world#)">>),
  ListConcat = clj_core:second(ListGenSym2),
  ListSecond = clj_core:second(ListConcat),
  ListThird = clj_core:third(ListConcat),
  true = clj_core:second(ListSecond) == clj_core:second(ListThird),

  ct:comment("Read unquote"),
  HelloSym = clj_core:symbol(<<"hello">>),
  ListWithMetaHelloSym = WrapWithMetaFun(HelloSym, undefined),
  ListWithMetaHelloSym = clj_reader:read(<<"`~hello">>),

  ct:comment("Use unquote splice not in list"),
  ok = try clj_reader:read(<<"`~@(hello)">>)
       catch _:_ -> ok end,

  ct:comment("Read list and empty list"),
  WithMetaListHello = clj_reader:read(<<"`(hello :world)">>),
  WithMetaListHello = clj_reader:read(<<"(clojure.core/with-meta"
                                        "  (clojure.core/concat"
                                        "    (clojure.core/list user/hello)"
                                        "    (clojure.core/list :world))"
                                        "  nil)">>),

  WithMetaEmptyList = clj_reader:read(<<"`()">>),
  WithMetaEmptyList = clj_reader:read(<<"(clojure.core/with-meta"
                                        "  (clojure.core/list)"
                                        "  nil)">>),

  ct:comment("Read map"),
  MapWithMeta = clj_reader:read(<<"`{hello :world}">>),
  MapWithMeta = clj_reader:read(<<"(clojure.core/with-meta"
                                  "  (clojure.core/apply"
                                  "    clojure.core/hash-map"
                                  "    (clojure.core/concat"
                                  "      (clojure.core/list user/hello)"
                                  "      (clojure.core/list :world)))"
                                  "  nil)">>),

  ct:comment("Read vector"),
  VectorWithMeta = clj_reader:read(<<"`[hello :world]">>),
  VectorWithMeta = clj_reader:read(<<"(clojure.core/with-meta"
                                     "  (clojure.core/apply"
                                     "    clojure.core/vector"
                                     "    (clojure.core/concat"
                                     "      (clojure.core/list user/hello)"
                                     "      (clojure.core/list :world)))"
                                     "  nil)">>),

  ct:comment("Read set"),
  SetWithMeta = clj_reader:read(<<"`#{hello :world}">>),
  SetWithMeta = clj_reader:read(<<"(clojure.core/with-meta"
                                  "  (clojure.core/apply"
                                  "    clojure.core/hash-set"
                                  "    (clojure.core/concat"
                                  "      (clojure.core/list :world)"
                                  "      (clojure.core/list user/hello)))"
                                  "  nil)">>),

  ct:comment("Read unquote-splice inside list"),
  WithMetaHelloWorldSup = clj_reader:read(<<"`(~@(hello world) :sup?)">>),
  WithMetaHelloWorldSup = clj_reader:read(<<"(clojure.core/with-meta"
                                            "  (clojure.core/concat"
                                            "    (hello world)"
                                            "    (clojure.core/list :sup?))"
                                            "  nil)">>),

  ct:comment("Read unquote inside list"),
  ListWithMetaHelloWorld = clj_reader:read(<<"`(~hello :world)">>),
  ListWithMetaHelloWorld = clj_reader:read(<<"(clojure.core/with-meta"
                                    "  (clojure.core/concat"
                                    "    (clojure.core/list hello)"
                                    "    (clojure.core/list :world))"
                                    "  nil)">>),

  {comments, ""}.

unquote(_Config) ->
  UnquoteSymbol = 'clojerl.Symbol':new(<<"clojure.core">>, <<"unquote">>),
  UnquoteSplicingSymbol = 'clojerl.Symbol':new(<<"clojure.core">>,
                                               <<"unquote-splicing">>),
  HelloWorldSymbol = 'clojerl.Symbol':new(<<"hello-world">>),

  ct:comment("Unquote"),
  ListUnquote = 'clojerl.List':new([UnquoteSymbol, HelloWorldSymbol]),
  ListUnquote = clj_reader:read(<<"~hello-world">>),

  ct:comment("Unquote splicing"),
  ListUnquoteSplicing = 'clojerl.List':new([UnquoteSplicingSymbol,
                                            HelloWorldSymbol]),
  ListUnquoteSplicing = clj_reader:read(<<"~@hello-world">>),

  ct:comment("Unquote nothing"),
  ok = try clj_reader:read(<<"~">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

list(_Config) ->
  HelloWorldKeyword = 'clojerl.Keyword':new(<<"hello-world">>),
  HelloWorldSymbol = 'clojerl.Symbol':new(<<"hello-world">>),

  ct:comment("Empty List"),
  EmptyList = 'clojerl.List':new([]),
  EmptyList = clj_reader:read(<<"()">>),

  ct:comment("List"),
  List = 'clojerl.List':new([HelloWorldKeyword, HelloWorldSymbol]),
  List = clj_reader:read(<<"(:hello-world hello-world)">>),

  ct:comment("List & space"),
  List = 'clojerl.List':new([HelloWorldKeyword, HelloWorldSymbol]),
  List = clj_reader:read(<<"(:hello-world hello-world )">>),

  ct:comment("List without closing paren"),
  ok = try clj_reader:read(<<"(1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

vector(_Config) ->
  HelloWorldKeyword = 'clojerl.Keyword':new(<<"hello-world">>),
  HelloWorldSymbol = 'clojerl.Symbol':new(<<"hello-world">>),

  ct:comment("Vector"),
  Vector = 'clojerl.Vector':new([HelloWorldKeyword, HelloWorldSymbol]),
  Vector = clj_reader:read(<<"[:hello-world hello-world]">>),

  ct:comment("Vector without closing bracket"),
  ok = try clj_reader:read(<<"[1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

map(_Config) ->
  HelloWorldKeyword = 'clojerl.Keyword':new(<<"hello-world">>),
  HelloWorldSymbol = 'clojerl.Symbol':new(<<"hello-world">>),

  ct:comment("Map"),
  Map = 'clojerl.Map':new([HelloWorldKeyword, HelloWorldSymbol,
                           HelloWorldSymbol, HelloWorldKeyword]),
  Map = clj_reader:read(<<"{:hello-world hello-world,"
                          " hello-world :hello-world}">>),

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
  HelloWorldKeyword = 'clojerl.Keyword':new(<<"hello-world">>),
  HelloWorldSymbol = 'clojerl.Symbol':new(<<"hello-world">>),

  ct:comment("Set"),
  Set = 'clojerl.Set':new([HelloWorldKeyword, HelloWorldSymbol]),
  Set = clj_reader:read(<<"#{:hello-world hello-world}">>),

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

  ct:comment("Char EOF"),
  ok = try clj_reader:read_all(<<"12 \\">>)
       catch _:_ -> ok
       end,

  ok = try clj_reader:read_all(<<"12 \\ ">>)
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
  EmptyList = clj_core:third(EmptyFn),

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
       catch _:<<"Nested #()s are not allowed">> -> ok end,

  {comments, ""}.

arg(_Config) ->
  ct:comment("Read % as a symbol"),
  ArgSymbol = clj_core:symbol(<<"%">>),
  ArgSymbol = clj_reader:read(<<"%">>),

  ct:comment("Read %1 as a symbol"),
  ArgOneSymbol = clj_core:symbol(<<"%1">>),
  ArgOneSymbol = clj_reader:read(<<"%1">>),

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
       catch _:<<"Arg literal must be %, %& or %integer">> -> ok end,

  erlang:erase(arg_env),

  {comments, ""}.

eval(_Config) ->
  ct:comment("Read eval 1"),
  1 = clj_reader:read(<<"#=1">>),

  ct:comment("Read eval (do 1)"),
  1 = clj_reader:read(<<"#=(do 1)">>),

  ct:comment("Read eval (str 1)"),
  <<"1">> = clj_reader:read(<<"#=(str 1)">>),

  {comments, ""}.

var(_Config) ->
  VarSymbol = 'clojerl.Symbol':new(<<"var">>),
  ListSymbol = 'clojerl.Symbol':new(<<"list">>),

  ct:comment(""),
  List = 'clojerl.List':new([VarSymbol, ListSymbol]),
  List = clj_reader:read(<<"#'list">>),

  {comments, ""}.

regex(_Config) ->
  {ok, Regex} = re:compile(<<".?el\\.lo">>),
  Regex = clj_reader:read(<<"#\".?el\\.lo\"">>),

  ct:comment("EOF: unterminated regex"),
  ok = try clj_reader:read_all(<<"#\"a*">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

unreadable_form(_Config) ->
  ct:comment("Read unreadable"),
  ok = try clj_reader:read(<<"#<1>">>)
       catch _:<<"Unreadable form">> -> ok
       end,

  {comments, ""}.

discard(_Config) ->
  [1] = clj_reader:read_all(<<"#_ :hello 1">>),

  1 = clj_reader:read(<<"#_ :hello 1">>),

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
  ReaderCond = {'clojerl.reader.ReaderConditional',
                #{list => clj_reader:read(<<"(1 2)">>),
                  splicing => false}},
  [ReaderCond, HelloKeyword] =
    clj_reader:read_all(<<"#?(1 2) :hello">>, PreserveOpts),

  ReaderCondSplice = {'clojerl.reader.ReaderConditional',
                      #{list => clj_reader:read(<<"(1 2)">>),
                        splicing => true}},
  ReaderCondSpliceVector = clj_core:vector([ReaderCondSplice, HelloKeyword]),
  ReaderCondSpliceVector =
    clj_reader:read(<<"[#?@(1 2) :hello]">>, PreserveOpts),

  ct:comment("EOF while reading character"),
  ok = try clj_reader:read(<<"#?">>, AllowOpts)
       catch _:<<"EOF while reading character">> -> ok
       end,

  ct:comment("Reader conditional not allowed"),
  ok = try clj_reader:read(<<"#?(:clj :whatever :clr :whateverrrr)">>)
       catch _:<<"Conditional read not allowed">> -> ok
       end,

  ct:comment("No list"),
  ok = try clj_reader:read(<<"#?:clj">>, AllowOpts)
       catch _:<<"read-cond body must be a list">> -> ok
       end,

  ct:comment("EOF: no feature matched"),
  ok = try clj_reader:read(<<"#?(:clj :whatever :clr :whateverrrr)">>,
                           AllowOpts)
       catch _:<<"EOF">> -> ok
       end,

  ct:comment("Uneven number of forms"),
  ok = try clj_reader:read(<<"#?(:one :two :three)">>, AllowOpts)
       catch _:<<"read-cond requires an even number of forms">> -> ok
       end,

  ct:comment("Splice not in list"),
  ok = try clj_reader:read(<<"#?@(:one [:two])">>, AllowOpts)
       catch _:<<"cond-splice not in list">> -> ok
       end,

  ct:comment("Splice in list but not sequential"),
  ok = try clj_reader:read(<<"[#?@(:clr :a :cljs :b) :c :d]">>,
                           AllowClrFeatureOpts)
       catch _:<<"Spliced form list in read-cond-splicing must "
                  "extend clojerl.ISequential">> -> ok
       end,

  {comments, ""}.

erl_fun(_Config) ->
  ct:comment("Read Erlang function"),
  SomeFunction1 = fun some:function/1,
  SomeFunction1 = clj_reader:read(<<"#:some/function[1]">>),

  SomeFunction2 = fun some:function/2,
  SomeFunction2 = clj_reader:read(<<"#: some/function[2]">>),

  ct:comment("Don't provide a fully qualified symbol"),
  ok = try clj_reader:read_all(<<"#:function[1]">>)
       catch _:_ -> ok
       end,

  ct:comment("Don't provide a vector"),
  ok = try clj_reader:read_all(<<"#:some/function">>)
       catch _:_ -> ok
       end,

  ct:comment("Don't provide a symbol"),
  ok = try clj_reader:read_all(<<"#:1[1]">>)
       catch _:_ -> ok
       end,

  ct:comment("Provide a vector with more than one element"),
  ok = try clj_reader:read_all(<<"#:some/function[1, 2]">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

unsupported_reader(_Config) ->
  ct:comment("Try unsupported reader"),
  ok = try clj_reader:read(<<"#-:something">>)
       catch _:_ -> ok
       end,

  {comments, ""}.
