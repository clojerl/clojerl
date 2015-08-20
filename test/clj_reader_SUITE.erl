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
    'cond'/1
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

meta(_Config) ->
  ct:comment("Meta keyword"),
  1 = clj_reader:read(<<"^:private 1">>),

  ct:comment("Meta symbol"),
  1 = clj_reader:read(<<"^private 1">>),

  ct:comment("Meta number"),
  1 = clj_reader:read(<<"^1 1">>),

  ct:comment("Reader meta number"),
  1 = clj_reader:read(<<"#^1 1">>),

  ct:comment("Meta without"),
  ok = try clj_reader:read(<<"^:private">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

syntax_quote(_Config) ->
  throw(unimplemented).

unquote(_Config) ->
  UnquoteSymbol = clj_symbol:new('clojure.core', 'unquote'),
  UnquoteSplicingSymbol = clj_symbol:new('clojure.core', 'unquote-splicing'),
  HelloWorldSymbol = clj_symbol:new('hello-world'),

  ct:comment("Unquote"),
  [UnquoteSymbol, HelloWorldSymbol] = clj_reader:read(<<"~hello-world">>),

  ct:comment("Unquote splicing"),
  [UnquoteSplicingSymbol, HelloWorldSymbol] = clj_reader:read(<<"~@hello-world">>),

  ct:comment("Unquote nothing"),
  ok = try clj_reader:read(<<"~">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

list(_Config) ->
  HelloWorldKeyword = clj_keyword:new('hello-world'),
  HelloWorldSymbol = clj_symbol:new('hello-world'),

  ct:comment("Empty List"),
  [] = clj_reader:read(<<"()">>),

  ct:comment("List"),
  [HelloWorldKeyword, HelloWorldSymbol] = clj_reader:read(<<"(:hello-world hello-world)">>),

  ct:comment("List without closing paren"),
  ok = try clj_reader:read(<<"(1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

vector(_Config) ->
  HelloWorldKeyword = clj_keyword:new('hello-world'),
  HelloWorldSymbol = clj_symbol:new('hello-world'),

  ct:comment("Vector"),
  Vector = clj_vector:new([HelloWorldKeyword, HelloWorldSymbol]),
  Vector = clj_reader:read(<<"[:hello-world hello-world]">>),

  ct:comment("Vector without closing bracket"),
  ok = try clj_reader:read(<<"[1 42.0">>)
       catch _:_ -> ok
       end,

  {comments, ""}.

map(_Config) ->
  HelloWorldKeyword = clj_keyword:new('hello-world'),
  HelloWorldSymbol = clj_symbol:new('hello-world'),

  ct:comment("Map"),
  #{HelloWorldKeyword := HelloWorldSymbol} = clj_reader:read(<<"{:hello-world hello-world}">>),

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
  HelloWorldKeyword = clj_keyword:new('hello-world'),
  HelloWorldSymbol = clj_symbol:new('hello-world'),

  ct:comment("Set"),
  Set = gb_sets:from_list([HelloWorldKeyword, HelloWorldSymbol]),
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

arg(_Config) ->
  ct:comment("Read argument"),
  clj_reader:read(<<"%1">>),

  {comments, ""}.


fn(_Config) ->
  ct:comment("Read anonymous fn"),
  clj_reader:read(<<"#(do 1)">>),

  {comments, ""}.

eval(_Config) ->
  ct:comment("Read eval"),
  clj_reader:read(<<"#=1">>),

  {comments, ""}.

var(_Config) ->
  VarSymbol = clj_symbol:new('var'),
  ListSymbol = clj_symbol:new('list'),

  ct:comment(""),
  [VarSymbol, ListSymbol] = clj_reader:read(<<"#'list">>),

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
  clj_reader:read(<<"#<1>">>),

  {comments, ""}.

discard(_Config) ->
  [1] = clj_reader:read_all(<<"#_:hello 1">>),

  1 = clj_reader:read(<<"#_:hello 1">>),

  {comments, ""}.

'cond'(_Config) ->
  ct:comment("Read cond"),
  clj_reader:read(<<"#? 1">>),

  {comments, ""}.
