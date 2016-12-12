-module(clojerl_Keyword_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([all/0, init_per_suite/1]).

-export([ hash/1
        , apply/1
        , name/1
        , str/1
        , write/1
        , read/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).


%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec hash(config()) -> result().
hash(_Config) ->
  Hash1 = 'clojerl.IHash':hash(hello),
  Hash1 = 'clojerl.IHash':hash(hello),

  Hash2 = 'clojerl.IHash':hash(world),

  true = Hash1 =/= Hash2,

  {comments, ""}.

-spec apply(config()) -> result().
apply(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  world = clj_core:apply(HelloKeyword, [#{HelloKeyword => world}]),

  ?NIL = clj_core:apply(HelloKeyword, [#{bla => ble}]),
  not_found = clj_core:apply(HelloKeyword, [#{bla => ble}, not_found]),

  ok = try
         clj_core:apply(HelloKeyword, [#{bla => ble}, not_found, extra]),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  ?NIL   = clj_core:namespace(HelloKeyword),
  <<"hello">> = clj_core:name(HelloKeyword),

  HelloWorldKeyword = clj_core:keyword(<<"hello">>, <<"world">>),
  <<"hello">> = clj_core:namespace(HelloWorldKeyword),
  <<"world">> = clj_core:name(HelloWorldKeyword),

  {comments, ""}.

-spec str(config()) -> result().
str(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  <<":hello">> = clj_core:str(HelloKeyword),

  HelloWorldKeyword = clj_core:keyword(<<"hello">>, <<"world">>),
  <<":hello/world">> = clj_core:str(HelloWorldKeyword),

  {comments, ""}.

-spec read(config()) -> result().
read(_Config) ->
  Pid = spawn_link(fun fake_io_loop/0),

  erlang:register(io_loop, Pid),
  <<"a">>        = 'erlang.io.IReader':read(io_loop),
  <<"aaaaa">>    = 'erlang.io.IReader':read(io_loop, 5),
  <<"get_line">> = 'erlang.io.IReader':read_line(io_loop),
  eof            = 'erlang.io.IReader':read(io_loop, 42),
  erlang:unregister(io_loop),

  ct:comment("Using standard_io maps sends requests to the group_leader"),
  GroupLeader = erlang:group_leader(),
  erlang:group_leader(Pid, self()),
  <<"a">>        = 'erlang.io.IReader':read(standard_io),
  <<"aaaaa">>    = 'erlang.io.IReader':read(standard_io, 5),
  <<"get_line">> = 'erlang.io.IReader':read_line(standard_io),
  eof            = 'erlang.io.IReader':read(standard_io, 42),
  erlang:group_leader(GroupLeader, self()),

  ct:comment("Read from a non-existing named process"),
  ok = try 'erlang.io.IReader':read(io_loop), error
       catch _:_ -> ok
       end,

  ok = try 'erlang.io.IReader':read_line(io_loop), error
       catch _:_ -> ok
       end,

  ct:comment("Unsupported skip"),
  ok = try 'erlang.io.IReader':skip(io_loop, 1), error
       catch _:_ -> ok
       end,

  ct:comment("Unsupported unread"),
  ok = try 'erlang.io.IReader':unread(io_loop, <<"hey">>), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec write(config()) -> result().
write(_Config) ->
  ok = ct:capture_start(),

  ct:comment("Write to stdout using the default atom"),
  'erlang.io.IWriter':write(standard_io, <<"hello">>),
  'erlang.io.IWriter':write(standard_io, <<" ">>),
  'erlang.io.IWriter':write(standard_io, <<"world!">>),
  "hello world!" = lists:flatten(ct:capture_get()),

  ct:comment("Write to stdout using an alias for the group leader"),
  true = erlang:register(leader, erlang:group_leader()),
  'erlang.io.IWriter':write(leader, <<"hello">>),
  'erlang.io.IWriter':write(leader, <<" ">>),
  'erlang.io.IWriter':write(leader, <<"world!">>),
  "hello world!" = lists:flatten(ct:capture_get()),
  true = erlang:unregister(leader),

  ct:comment("Write to a non-existing named process"),
  ok = try 'erlang.io.IWriter':write(leader, <<"hello">>), error
       catch _:_ -> ok
       end,

  ok = ct:capture_stop(),

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  ct:comment("Find existing leywords"),
  str = 'clojerl.Keyword':find(<<"str">>),
  'hello/world' = 'clojerl.Keyword':find(<<"hello">>, <<"world">>),

  ct:comment("Find existing leywords"),
  ?NIL = 'clojerl.Keyword':find(<<"123456">>),
  ?NIL = 'clojerl.Keyword':find(<<"123456">>, <<"123456">>),
  ?NIL = 'clojerl.Keyword':find(?NIL, <<"123456">>),

  ct:comment("Use all new clauses"),
  hello = 'clojerl.Keyword':?CONSTRUCTOR(hello),
  hello = 'clojerl.Keyword':?CONSTRUCTOR(clj_core:symbol(<<"hello">>)),
  hello = 'clojerl.Keyword':?CONSTRUCTOR( ?NIL
                                        , clj_core:symbol(<<"hello">>)
                                        ),

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Fake simple IO loop
%%------------------------------------------------------------------------------

-spec fake_io_loop() -> ok.
fake_io_loop() ->
  receive
    {io_request, From, ReplyAs, Request} ->
      From ! {io_reply, ReplyAs, fake_reply(Request)},
      fake_io_loop();
    {From, Ref, close} ->
      From ! {Ref, ok},
      ok;
    _Unknown ->
      fake_io_loop()
  end.

-spec fake_reply(tuple()) -> binary().
fake_reply({get_chars, _, _, 42}) ->
  eof;
fake_reply({get_chars, _, _, N}) ->
  list_to_binary(lists:flatten(repeat("a", N)));
fake_reply({get_line, _, _}) ->
  <<"get_line">>;
fake_reply({get_until, _, _, _, _, _}) ->
  <<"get_until">>.

-spec repeat(string(), integer()) -> iolist().
repeat(X, N) ->
  lists:map(fun(_) -> X end, lists:seq(1, N)).
