-module('clojerl.ExceptionInfo').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IError').
-behavior('clojerl.IExceptionInfo').
-behavior('clojerl.IHash').
-behavior('clojerl.Stringable').

-export([?CONSTRUCTOR/2, ?CONSTRUCTOR/3]).

-export([equiv/2]).
-export([message/1]).
-export([ data/1
        , cause/1
        ]).
-export([hash/1]).
-export([str/1]).

-type type() :: #?TYPE{data :: #{ message   => binary()
                                , data  => any()
                                , cause => any()
                                }}.

-spec ?CONSTRUCTOR(binary(), any()) -> type().
?CONSTRUCTOR(Message, Data) when is_binary(Message) ->
  ?CONSTRUCTOR(Message, Data, ?NIL).

-spec ?CONSTRUCTOR(binary(), any(), any()) -> type().
?CONSTRUCTOR(Message, Data, Cause) when is_binary(Message) ->
  #?TYPE{data = #{message => Message, data => Data, cause => Cause}}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IEquiv

equiv(#?TYPE{name = ?M, data = X}, #?TYPE{name = ?M, data = X}) ->
  true;
equiv(_, _) ->
  false.

%% clojerl.IError

message(#?TYPE{name = ?M, data = #{message := Message}}) ->
  Message.

%% clojerl.IExceptionInfo

data(#?TYPE{name = ?M, data = #{data := Data}}) ->
  Data.

cause(#?TYPE{name = ?M, data = #{cause := Cause}}) ->
  Cause.

%% clojerl.IHash

hash(#?TYPE{name = ?M, data = Data}) ->
  erlang:phash2(Data).

%% clojerl.Stringable

str(#?TYPE{name = ?M, data = #{message := Message, data := Data}}) ->
  TypeBin = erlang:atom_to_binary(?MODULE, utf8),
  DataBin = clj_rt:str(Data),
  <<TypeBin/binary, ": ", Message/binary, " ", DataBin/binary>>.
