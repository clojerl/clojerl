-module('erlang.io.PushbackReader').

-include("clojerl.hrl").

-behaviour('erlang.io.Closeable').
-behaviour('clojerl.Stringable').
-behaviour('erlang.io.IReader').

-export([?CONSTRUCTOR/1, at_line_start/1]).
-export([ start_link/1
        , init/1
        , loop/1
        , skip/3
        ]).

-export([close/1]).
-export([  read/1
        ,  read/2
        ,  read_line/1
        ,  skip/2
        ,  unread/2
        ]).
-export([str/1]).

-type type() :: #?TYPE{data :: pid()}.

-spec ?CONSTRUCTOR('erlang.io.IReader':type()) -> type().
?CONSTRUCTOR(Reader) ->
  #?TYPE{data = start_link(Reader)}.

-spec at_line_start('erlang.io.IReader':type()) -> type().
at_line_start(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, at_line_start) of
    {error, _} -> error(<<"Can't determine if at line start">>);
    Result     -> Result
  end.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

close(#?TYPE{name = ?M, data = Pid}) ->
  case send_command(Pid, close) of
    {error, _} ->
      TypeName = atom_to_binary(?MODULE, utf8),
      error(<<"Couldn't close ", TypeName/binary>>);
    _ ->
      ?NIL
  end.

str(#?TYPE{name = ?M, data = Pid}) ->
  TypeName = atom_to_binary(?MODULE, utf8),
  <<"<", PidStr/binary>> = erlang:list_to_binary(erlang:pid_to_list(Pid)),
  <<"#<", TypeName/binary, " ", PidStr/binary>>.

read(#?TYPE{name = ?M, data = Pid}) ->
  io:get_chars(Pid, "", 1).

read(#?TYPE{name = ?M, data = Pid}, Length) ->
  io:get_chars(Pid, "", Length).

read_line(#?TYPE{name = ?M, data = Pid}) ->
  io:request(Pid, {get_line, unicode, ""}).

skip(#?TYPE{name = ?M, data = Pid}, Length) ->
  io:request(Pid, {get_until, unicode, "", ?MODULE, skip, [Length]}).

unread(#?TYPE{name = ?M, data = Pid} = Reader, Str) ->
  case send_command(Pid, {unread, Str}) of
    {error, Reason} ->
      TypeName  = atom_to_binary(?MODULE, utf8),
      ReasonBin = clj_rt:str(Reason),
      error(<<"Couldn't unread to ", TypeName/binary, ": ", ReasonBin/binary>>);
    ok ->
      Reader
  end.

%%------------------------------------------------------------------------------
%% IO server
%%
%% Implementation of a subset of the io protocol in order to only support
%% writing operations.
%%------------------------------------------------------------------------------

-type state() :: #{ reader        => 'erlang.io.IReader':type()
                  , buffer        => binary()
                  , at_line_start => boolean()
                  }.

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

-spec start_link('erlang.io.IReader':type()) -> pid().
start_link(Reader) ->
  spawn_link(?MODULE, init, [Reader]).

-spec init('erlang.io.IReader':type()) -> no_return().
init(Reader) ->
  State = #{ reader        => Reader
           , buffer        => <<>>
           , at_line_start => true
           },
  ?MODULE:loop(State).

-spec loop(state()) -> ok.
loop(State) ->
  receive
    {io_request, From, ReplyAs, Request} ->
      {Reply, NewState} = request(Request, State),
      reply(From, ReplyAs, Reply),
      ?MODULE:loop(NewState);
    {From, Ref, close} ->
      #{reader := Reader} = State,
      Result = try 'erlang.io.Closeable':close(Reader)
               catch error:Error -> {error, Error}
               end,
      From ! {Ref, Result};
    {From, Ref, {unread, Str}} ->
      {Result, NewState} = unread_buffer(State, Str),
      From ! {Ref, Result},
      ?MODULE:loop(NewState);
    {From, Ref, at_line_start} ->
      #{at_line_start := AtLineStart} = State,
      From ! {Ref, AtLineStart},
      ?MODULE:loop(State);
    _Unknown ->
      ?MODULE:loop(State)
  end.

reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

request({get_chars, Encoding, _Prompt, N}, State) ->
  maybe_encode_result(Encoding, get_chars(N, State));
request({get_line, Encoding, _Prompt}, State) ->
  maybe_encode_result(Encoding, get_line(State));
request({get_until, Encoding, _Prompt, Module, Function, Xargs}, State) ->
  maybe_encode_result(Encoding, get_until(Module, Function, Xargs, State));
request(_Other, State) ->
  {{error, request}, State}.

-spec maybe_encode_result(atom(), {term(), state()}) -> {term(), state()}.
maybe_encode_result(Encoding, {Result, NewState}) when is_binary(Result) ->
  { unicode:characters_to_binary(Result, unicode, Encoding)
  , update_at_line_start(Result, NewState)
  };
maybe_encode_result(_, X) ->
  X.

-spec update_at_line_start(binary(), state()) -> state().
update_at_line_start(Result, State) ->
  State#{at_line_start := 'clojerl.String':ends_with(Result, <<"\n">>)}.

-spec get_chars(integer(), state()) -> {binary() | eof, state()}.
get_chars(N, #{reader := Reader, buffer := <<>>} = State) ->
  {'erlang.io.IReader':read(Reader, N), State};
get_chars(1, #{buffer := <<Ch/utf8, Str/binary>>} = State) ->
  {<<Ch/utf8>>, State#{buffer => Str}};
get_chars(N, State) ->
  do_get_chars(N, State, <<>>).

-spec do_get_chars(integer(), state(), binary()) -> {binary(), state()}.
do_get_chars(0, State, Result) ->
  {Result, State};
do_get_chars(N, #{buffer := <<Ch/utf8, Rest/binary>>} = State, Result) ->
  do_get_chars(N - 1, State#{buffer := Rest}, <<Result/binary, Ch/utf8>>);
do_get_chars(N, #{reader := Reader} = State, Result) ->
  case 'erlang.io.IReader':read(Reader, N) of
    eof ->
      {Result, State};
    Str ->
      {<<Result/binary, Str/binary>>, State}
  end.

-spec get_line(state()) -> {binary() | eof, state()}.
get_line(State) ->
  do_get_line(State, <<>>).

-spec do_get_line(state(), binary()) -> {binary() | eof, state()}.
do_get_line(#{buffer := <<"\r\n"/utf8, RestStr/binary>>} = State, Result) ->
  {Result, State#{buffer := RestStr}};
do_get_line(#{buffer := <<"\n"/utf8, RestStr/binary>>} = State, Result) ->
  {Result, State#{buffer := RestStr}};
do_get_line(#{buffer := <<"\r"/utf8, RestStr/binary>>} = State, Result) ->
  {Result, State#{buffer := RestStr}};
do_get_line(#{buffer := <<Ch/utf8, RestStr/binary>>} = State, Result) ->
  do_get_line(State#{buffer := RestStr}, <<Result/binary, Ch/utf8>>);
do_get_line(#{reader := Reader} = State, Result) ->
  case 'erlang.io.IReader':read_line(Reader) of
    eof when Result =:= <<>> ->
      {eof, State};
    Str ->
      {<<Result/binary, Str/binary>>, State}
  end.

-spec get_until(module(), atom(), list(), term()) ->
  {term(), state()}.
get_until(Module, Function, XArgs, State) ->
  case apply(Module, Function, [State, ?NIL | XArgs]) of
    {done, Result, NewStr} -> {Result, NewStr};
    {more, NewState} -> get_until(Module, Function, XArgs, NewState)
  end.

-spec skip(state() | {cont, integer(), state()}, term(), integer()) ->
  {more, {cont, integer(), binary()}} | {done, integer(), binary()}.
skip(State, _Data, Length) when is_map(State) ->
  {more, {cont, Length, State}};
skip({cont, 0, State}, _Data, Length) ->
  {done, Length, State};
skip({cont, Length, #{buffer := <<>>} = State}, _Data, _Length) ->
  #{reader := Reader} = State,
  {done, 'erlang.io.IReader':skip(Reader, Length), State};
skip( {cont, Length, #{buffer := <<_/utf8, RestStr/binary>>} = State}
    , _Data
    , _Length
    ) ->
  {more, {cont, Length - 1, State#{buffer := RestStr}}}.

-spec unread_buffer(state(), binary()) -> {ok | {error, term()}, state()}.
unread_buffer(State = #{buffer := Buffer}, Str) ->
  try
    {ok, State#{buffer := <<Str/binary, Buffer/binary>>}}
  catch
    _:Reason ->
      {{error, Reason}, State}
  end.
