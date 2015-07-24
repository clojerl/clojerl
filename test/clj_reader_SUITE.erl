-module(clj_reader_SUITE).

-export([all/0]).

-export(
   [
    number/1,
    string/1,
    keyword/1,
    symbol/1,
    comment/1,
    quote/1,
    deref/1
   ]
  ).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

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
  Env = #{ns => 'some-ns'},

  Keyword1 = clj_keyword:new('hello-world'),
  Keyword1 = clj_reader:read(<<":hello-world">>, Env),

  Keyword2 = clj_keyword:new('some-ns', 'hello-world'),
  Keyword2 = clj_reader:read(<<"::hello-world">>, Env),

  Keyword3 = clj_keyword:new('another-ns', 'hello-world'),
  Keyword3 = clj_reader:read(<<":another-ns/hello-world">>, Env),

  Keyword4 = clj_keyword:new('/'),
  Keyword4 = clj_reader:read(<<":/">>, Env),

  Keyword5 = clj_keyword:new('some-ns', '/'),
  Keyword5 = clj_reader:read(<<":some-ns//">>, Env),

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
  Symbol1 = clj_symbol:new('hello-world'),
  Symbol1 = clj_reader:read(<<"hello-world">>),

  Symbol2 = clj_symbol:new('some-ns', 'hello-world'),
  Symbol2 = clj_reader:read(<<"some-ns/hello-world">>),

  Symbol3 = clj_symbol:new('another-ns', 'hello-world'),
  Symbol3 = clj_reader:read(<<"another-ns/hello-world">>),

  Symbol4 = clj_symbol:new('some-ns', '/'),
  Symbol4 = clj_reader:read(<<"some-ns//">>),

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
  BlaKeyword = clj_keyword:new(bla),

  ct:comment("Error: single semi-colon"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 ; coment\n :bla ">>),

  ct:comment("Error: two semi-colon"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 ;; coment\n :bla ">>),

  ct:comment("Error: a bunch semi-colon"),
  [1, BlaKeyword] = clj_reader:read_all(<<"1 ;;;; coment\n :bla ">>),

  {comments, ""}.

quote(_Config) ->
  QuoteSymbol = clj_symbol:new(quote),
  ListSymbol = clj_symbol:new(list),

  ct:comment("Quote number"),
  [QuoteSymbol, 1] = clj_reader:read(<<"'1">>),

  ct:comment("Quote symbol"),
  [QuoteSymbol, ListSymbol] = clj_reader:read(<<"'list">>),

  ct:comment("Quote space symbol  "),
  [QuoteSymbol, ListSymbol] = clj_reader:read(<<"' list">>),

  ct:comment("Error: only provide ' "),
  ok = try clj_reader:read(<<"'">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

deref(_Config) ->
  DerefSymbol = clj_symbol:new(deref),
  ListSymbol = clj_symbol:new(list),

  ct:comment("Deref number :P"),
  [DerefSymbol, 1] = clj_reader:read(<<"@1">>),

  ct:comment("Deref symbol :P"),
  [DerefSymbol, ListSymbol] = clj_reader:read(<<"@list">>),

  ct:comment("Deref symbol :P and read other stuff"),
  [[DerefSymbol, ListSymbol], 42.0] = clj_reader:read_all(<<"@list 42.0">>),

  ct:comment("Error: only provide @ "),
  ok = try clj_reader:read(<<"@">>)
       catch _:_ -> ok
       end,

  {comments, ""}.
