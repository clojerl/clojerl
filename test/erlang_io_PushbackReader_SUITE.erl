-module(erlang_io_PushbackReader_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([ str/1
        , read/1
        , read_line/1
        , skip/1
        , unread/1
        , at_line_start/1
        , close/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config0) ->
  Config = clj_test_utils:init_per_suite(Config0),
  file:write_file(<<"tmp">>, <<"Hello world!\n">>),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  file:delete(<<"tmp">>),
  Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec str(config()) -> result().
str(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),

  Str   = clj_rt:str(Reader),
  Regex = <<"#<erlang.io.PushbackReader \\d+\\.\\d+\\.\\d+>">>,
  match = re:run(Str, Regex, [{capture, none}]),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  {comments, ""}.

-spec read(config()) -> result().
read(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),

  <<"Hello">> = 'erlang.io.IReader':read(Reader, 5),
  Reader      = 'erlang.io.IPushbackReader':unread(Reader, <<"Hello">>),
  <<"H">>     = 'erlang.io.IReader':read(Reader),
  <<"ello ">> = 'erlang.io.IReader':read(Reader, 5),
  <<"world">> = 'erlang.io.IReader':read(Reader, 5),
  <<"!\n">>   = 'erlang.io.IReader':read(Reader, 2),
  Reader      = 'erlang.io.IPushbackReader':unread(Reader, <<"yeah">>),
  <<"yeah">>  = 'erlang.io.IReader':read(Reader, 10),
  eof         = 'erlang.io.IReader':read(Reader),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  {comments, ""}.

-spec read_line(config()) -> result().
read_line(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),

  <<"Hello world!\n">> = 'erlang.io.IReader':read_line(Reader),
  Reader = 'erlang.io.IPushbackReader':unread(Reader, <<"hello\r">>),
  <<"hello">> = 'erlang.io.IReader':read_line(Reader),
  Reader = 'erlang.io.IPushbackReader':unread(Reader, <<"hello\r\n">>),
  <<"hello">> = 'erlang.io.IReader':read_line(Reader),
  Reader = 'erlang.io.IPushbackReader':unread(Reader, <<"hello\n">>),
  <<"hello">> = 'erlang.io.IReader':read_line(Reader),

  eof = 'erlang.io.IReader':read_line(Reader),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  {comments, ""}.

-spec skip(config()) -> result().
skip(_Config) ->
  StrReader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"Hello world!\n">>),
  Reader    = 'erlang.io.PushbackReader':?CONSTRUCTOR(StrReader),

  6            = 'erlang.io.IReader':skip(Reader, 6), %% Skipped "Hello "
  <<"world!">> = 'erlang.io.IReader':read_line(Reader),
  Reader       = 'erlang.io.IPushbackReader':unread(Reader, <<"world!">>),
  3            = 'erlang.io.IReader':skip(Reader, 3), %% Skipped "wor"
  <<"ld">>     = 'erlang.io.IReader':read(Reader, 2),
  eof          = 'erlang.io.IReader':skip(Reader, 3), %% Skipped "!\n"
  eof          = 'erlang.io.IReader':read_line(Reader),
  eof          = 'erlang.io.IReader':skip(Reader, 1),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  {comments, ""}.

-spec unread(config()) -> result().
unread(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),

  <<"Hello">> = 'erlang.io.IReader':read(Reader, 5),
  <<" ">>     = 'erlang.io.IReader':read(Reader),
  Reader      = 'erlang.io.IPushbackReader':unread(Reader, <<"_ _">>),
  <<"_ _">>   = 'erlang.io.IReader':read(Reader, 3),
  <<"world">> = 'erlang.io.IReader':read(Reader, 5),
  <<"!\n">>   = 'erlang.io.IReader':read(Reader, 2),
  eof         = 'erlang.io.IReader':read(Reader),

  ok = try 'erlang.io.IPushbackReader':unread(Reader, something), error
       catch _:_ -> ok
       end,

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  {comments, ""}.

-spec at_line_start(config()) -> result().
at_line_start(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),

  true         = 'erlang.io.PushbackReader':at_line_start(Reader),
  <<"Hello ">> = 'erlang.io.IReader':read(Reader, 6),
  false        = 'erlang.io.PushbackReader':at_line_start(Reader),
  <<"world">>  = 'erlang.io.IReader':read(Reader, 5),
  <<"!\n">>    = 'erlang.io.IReader':read(Reader, 2),
  true         = 'erlang.io.PushbackReader':at_line_start(Reader),
  eof          = 'erlang.io.IReader':read(Reader),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  ct:comment("At line start fails"),
  FakePid = spawn_link(fun fake_loop/0),
  FakeReader = Reader#{pid => FakePid},
  ok = try 'erlang.io.PushbackReader':at_line_start(FakeReader), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  ok = try ?NIL = 'erlang.io.ICloseable':close(Reader), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  File   = 'erlang.io.File':open(<<"tmp">>),
  Reader = 'erlang.io.PushbackReader':?CONSTRUCTOR(File),
  Pid    = maps:get(pid, Reader),

  ct:comment("Send an unexpected message"),
  Pid ! {},

  ct:comment("Send an unexpected io_request"),
  {error, request} = io:request(Pid, {io_request, self(), ref, unexpected}),

  ct:comment("Generate error when the underlying reader is closed"),
  ?NIL       = 'erlang.io.ICloseable':close(File),
  {error, _} = 'erlang.io.IReader':skip(Reader, 1),

  ?NIL = 'erlang.io.ICloseable':close(Reader),

  ct:comment("Generate error when closing a PushbackReader that wraps a file"),
  File1   = 'erlang.io.File':open(<<"tmp">>),
  Reader1 = 'erlang.io.PushbackReader':?CONSTRUCTOR(File1),
  meck:new(file, [passthrough, unstick]),
  try
    meck:expect(file, close, fun(_) -> {error, fake} end),
    ok = try 'erlang.io.ICloseable':close(Reader1), error
         catch _:_ -> ok
         end
  after
    meck:unload(file)
  end,

  {comments, ""}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec fake_loop() -> ok.
fake_loop() ->
  receive
    {From, Ref, at_line_start} ->
      From ! {Ref, {error, failed}},
      fake_loop()
  end.
