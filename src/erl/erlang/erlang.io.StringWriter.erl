-module('erlang.io.StringWriter').

-include("clojerl.hrl").

-behavior('clojerl.ICounted').
-behavior('clojerl.IStringable').
-behavior('erlang.io.ICloseable').
-behavior('erlang.io.IWriter').

-export([?CONSTRUCTOR/0, ?CONSTRUCTOR/1]).
-export([ init/1
        , loop/1
        ]).

-export([count/1]).
-export([str/1]).
-export([close/1]).
-export([ write/2
        , write/3
        ]).

-export([delete/3]).

-export_type([type/0]).
-type type() :: #{ ?TYPE => ?M
                 , pid   => pid()
                 }.

-spec ?CONSTRUCTOR() -> type().
?CONSTRUCTOR() ->
  ?CONSTRUCTOR(<<>>).

-spec ?CONSTRUCTOR(binary()) -> type().
?CONSTRUCTOR(Str) ->
  #{ ?TYPE => ?M
   , pid   => start_link(Str)
   }.

-spec delete(type(), pos_integer(), pos_integer()) -> type().
delete(SW = #{?TYPE := ?M, pid := Pid}, Start, End) ->
  case send_command(Pid, {delete, Start, End}) of
    {error, _} -> error(<<"Couldn't delete range in erlang.io.StringWriter">>);
    ok         -> SW
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

count(#{?TYPE := ?M, pid := Pid}) ->
  case send_command(Pid, count) of
    {error, _} -> error(<<"Couldn't get length from erlang.io.StringWriter">>);
    Count      -> Count
  end.

str(#{?TYPE := ?M, pid := Pid}) ->
  case send_command(Pid, str) of
    {error, _} -> error(<<"Couldn't get string from erlang.io.StringWriter">>);
    Str        -> Str
  end.

close(#{?TYPE := ?M, pid := Pid}) ->
  case send_command(Pid, close) of
    {error, _} -> error(<<"Couldn't close erlang.io.StringWriter">>);
    _          -> ?NIL
  end.

write(#{?TYPE := ?M, pid := Pid} = SW, Str) ->
  ok = io:put_chars(Pid, Str),
  SW.

write(#{?TYPE := ?M, pid := Pid} = SW, Format, Values) ->
  ok = io:fwrite(Pid, Format, clj_rt:to_list(Values)),
  SW.

%%------------------------------------------------------------------------------
%% IO server
%%
%% Implementation of a subset of the io protocol in order to only support
%% writing operations.
%%------------------------------------------------------------------------------

-spec send_command(pid(), any()) -> any().
send_command(Pid, Cmd) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, Cmd},
  receive
    {Ref, Result} ->
      erlang:demonitor(Ref, [flush]),
      Result;
    {'DOWN', Ref, _, _, _} ->
      {error, terminated}
  end.

start_link(Str) ->
  spawn_link(?MODULE, init, [Str]).

init(Str) -> ?MODULE:loop(Str).

loop(Str0) ->
  receive
    {io_request, From, ReplyAs, Request} ->
      {Reply, Str1} = request(Request, Str0),
      reply(From, ReplyAs, Reply),
      ?MODULE:loop(Str1);
    {From, Ref, close} ->
      From ! {Ref, ok};
    {From, Ref, Cmd} ->
      Str2 = try
               {Reply, Str1} = commands(Cmd, Str0),
               From ! {Ref, Reply},
               Str1
             catch _:Reason ->
                 From ! {Ref, {error, Reason}},
                 Str0
             end,
      ?MODULE:loop(Str2);
    _Unknown ->
      ?MODULE:loop(Str0)
  end.

reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, Str) ->
  EncodedChars = unicode:characters_to_list(Chars, Encoding),
  CharsBin = unicode:characters_to_binary(EncodedChars),
  {ok, <<Str/binary, CharsBin/binary>>};
request({put_chars, Encoding, Module, Function, Args}, State) ->
  try
    request({put_chars, Encoding, apply(Module, Function, Args)}, State)
  catch
    _:_ -> {{error, Function}, State}
  end;
request(_Other, State) ->
  {{error, request}, State}.

-spec commands(tuple(), binary()) -> {any(), binary()}.
commands(count, Str) ->
  {'clojerl.String':count(Str), Str};
commands({delete, Start, End}, Str) ->
  First  = 'clojerl.String':substring(Str, 0, Start),
  Second = 'clojerl.String':substring(Str, End),
  {ok, <<First/binary, Second/binary>>};
commands(str, Str) ->
  {Str, Str}.
