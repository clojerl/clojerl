-module(clojerl_Keyword_SUITE).

-export([all/0, init_per_suite/1]).

-export([ hash/1
        , invoke/1
        , name/1
        , str/1
        , write/1
        , read/1
        , complete_coverage/1
        ]).

-type config() :: list().
-type result() :: {comments, string()}.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(clojerl),
  Config.

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

-spec invoke(config()) -> result().
invoke(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  world = clj_core:invoke(HelloKeyword, [#{HelloKeyword => world}]),

  not_found = clj_core:invoke(HelloKeyword, [#{bla => ble}, not_found]),

  ok = try
         clj_core:invoke(HelloKeyword, [#{bla => ble}, not_found, extra]),
         error
       catch _:_ ->
           ok
       end,

  {comments, ""}.

-spec name(config()) -> result().
name(_Config) ->
  HelloKeyword = clj_core:keyword(<<"hello">>),
  undefined   = clj_core:namespace(HelloKeyword),
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
  <<"a">>        = 'clojerl.IReader':read(io_loop),
  <<"aaaaa">>    = 'clojerl.IReader':read(io_loop, 5),
  <<"get_line">> = 'clojerl.IReader':read_line(io_loop),
  erlang:unregister(io_loop),

  ct:comment("Read from a non-existing named process"),
  ok = try 'clojerl.IReader':read(io_loop), error
       catch _:_ -> ok
       end,

  ok = try 'clojerl.IReader':read_line(io_loop), error
       catch _:_ -> ok
       end,

  ct:comment("Unsupported skip"),
  ok = try 'clojerl.IReader':skip(io_loop, 1), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec write(config()) -> result().
write(_Config) ->
  ok = ct:capture_start(),

  ct:comment("Write to stdout using the default atom"),
  'clojerl.IWriter':write(standard_io, <<"hello">>),
  'clojerl.IWriter':write(standard_io, <<" ">>),
  'clojerl.IWriter':write(standard_io, <<"world!">>),
  "hello world!" = lists:flatten(ct:capture_get()),

  ct:comment("Write to stdout using an alias for the group leader"),
  true = erlang:register(leader, erlang:group_leader()),
  'clojerl.IWriter':write(leader, <<"hello">>),
  'clojerl.IWriter':write(leader, <<" ">>),
  'clojerl.IWriter':write(leader, <<"world!">>),
  "hello world!" = lists:flatten(ct:capture_get()),
  true = erlang:unregister(leader),

  ct:comment("Write to a non-existing named process"),
  ok = try 'clojerl.IWriter':write(leader, <<"hello">>), error
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
  undefined = 'clojerl.Keyword':find(<<"123456">>),
  undefined = 'clojerl.Keyword':find(<<"123456">>, <<"123456">>),

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
fake_reply({get_chars, _, _, N}) ->
  lists:flatten(repeat("a", N));
fake_reply({get_line, _, _}) ->
  "get_line";
fake_reply({get_until, _, _, _, _, _}) ->
  "get_until".

-spec repeat(string(), integer()) -> iolist().
repeat(X, N) ->
  lists:map(fun(_) -> X end, lists:seq(1, N)).
