-module(erlang_io_StringReader_SUITE).

-include("clojerl.hrl").
-include("clj_test_utils.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ str/1
        , read/1
        , read_line/1
        , skip/1
        , close/1
        , complete_coverage/1
        ]).

-spec all() -> [atom()].
all() -> clj_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> clj_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

-spec str(config()) -> result().
str(_Config) ->
  Reader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello">>),

  Str   = clj_rt:str(Reader),
  Regex = <<"#<erlang.io.StringReader \\d+\\.\\d+\\.\\d+>">>,
  match = re:run(Str, Regex, [{capture, none}]),

  ?NIL = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec read(config()) -> result().
read(_Config) ->
  Reader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello">>),

  <<"h">> = 'erlang.io.IReader':read(Reader),
  <<"e">> = 'erlang.io.IReader':read(Reader),
  <<"l">> = 'erlang.io.IReader':read(Reader),
  <<"l">> = 'erlang.io.IReader':read(Reader),
  <<"o">> = 'erlang.io.IReader':read(Reader),
  eof     = 'erlang.io.IReader':read(Reader),
  eof     = 'erlang.io.IReader':read(Reader),

  ?NIL = 'erlang.io.Closeable':close(Reader),

  Reader2 = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello world!">>),

  <<"he">>      = 'erlang.io.IReader':read(Reader2, 2),
  <<"llo">>     = 'erlang.io.IReader':read(Reader2, 3),
  <<" ">>       = 'erlang.io.IReader':read(Reader2),
  <<"world!">>  = 'erlang.io.IReader':read(Reader2, 7),
  eof           = 'erlang.io.IReader':read(Reader2),

  ?NIL = 'erlang.io.Closeable':close(Reader2),

  {comments, ""}.

-spec read_line(config()) -> result().
read_line(_Config) ->
  Reader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello\nworld!\n">>),

  <<"hello">>  = 'erlang.io.IReader':read_line(Reader),
  <<"world!">> = 'erlang.io.IReader':read_line(Reader),
  eof          = 'erlang.io.IReader':read_line(Reader),

  ?NIL = 'erlang.io.Closeable':close(Reader),

  Reader2 = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello\rworld!\rHello.">>),

  <<"hello">>  = 'erlang.io.IReader':read_line(Reader2),
  <<"world!">> = 'erlang.io.IReader':read_line(Reader2),
  <<"Hello.">> = 'erlang.io.IReader':read_line(Reader2),
  eof          = 'erlang.io.IReader':read_line(Reader2),

  ?NIL = 'erlang.io.Closeable':close(Reader2),

  Reader3 = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello\r\nworld!\r\n">>),

  <<"hello">>  = 'erlang.io.IReader':read_line(Reader3),
  <<"world!">> = 'erlang.io.IReader':read_line(Reader3),
  eof          = 'erlang.io.IReader':read_line(Reader3),

  ?NIL = 'erlang.io.Closeable':close(Reader3),

  {comments, ""}.

-spec skip(config()) -> result().
skip(_Config) ->
  Reader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello\nworld!\n">>),

  6            = 'erlang.io.IReader':skip(Reader, 6),
  <<"world!">> = 'erlang.io.IReader':read_line(Reader),
  eof          = 'erlang.io.IReader':read_line(Reader),
  eof          = 'erlang.io.IReader':skip(Reader, 1),

  ?NIL = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.

-spec close(config()) -> result().
close(_Config) ->
  Reader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello\nworld!\n">>),

  ?NIL = 'erlang.io.Closeable':close(Reader),

  ok = try ?NIL = 'erlang.io.Closeable':close(Reader), error
       catch _:_ -> ok
       end,

  {comments, ""}.

-spec complete_coverage(config()) -> result().
complete_coverage(_Config) ->
  Reader = 'erlang.io.StringReader':?CONSTRUCTOR(<<"hello\nworld!\n">>),
  Pid    = Reader#?TYPE.data,

  ct:comment("Send an unexpected message"),
  Pid ! {},

  ct:comment("Send an unexpected io_request"),
  {error, request} = io:request(Pid, {io_request, self(), ref, unexpected}),

  ?NIL = 'erlang.io.Closeable':close(Reader),

  {comments, ""}.
