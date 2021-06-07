-module('clojerl.ExceptionInfo').

-include("clojerl.hrl").

-behavior('clojerl.IEquiv').
-behavior('clojerl.IError').
-behavior('clojerl.IExceptionInfo').
-behavior('clojerl.IHash').
-behavior('clojerl.IStringable').

-export([?CONSTRUCTOR/2, ?CONSTRUCTOR/3]).

-export([equiv/2]).
-export([message/1]).
-export([ data/1
        , cause/1
        ]).
-export([hash/1]).
-export([str/1]).

-type type() :: #{ ?TYPE   => ?M
                 , message => binary()
                 , data    => any()
                 , cause   => any()
                 }.

-spec ?CONSTRUCTOR(binary(), any()) -> type().
?CONSTRUCTOR(Message, Data) when is_binary(Message) ->
  ?CONSTRUCTOR(Message, Data, ?NIL).

-spec ?CONSTRUCTOR(binary(), any(), any()) -> type().
?CONSTRUCTOR(_Message, ?NIL, _Cause) ->
  ErrorMessage = <<"Additional data must be non-nil.">>,
  erlang:error('clojerl.BadArgumentError':?CONSTRUCTOR(ErrorMessage));
?CONSTRUCTOR(Message, Data, Cause) when is_binary(Message) ->
  #{ ?TYPE   => ?M
   , message => Message
   , data    => Data
   , cause   => Cause
   }.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

%% clojerl.IEquiv

equiv( #{?TYPE := ?M, message := Msg, data := Data, cause := Cause}
     , #{?TYPE := ?M, message := Msg, data := Data, cause := Cause}
     ) ->
  true;
equiv(_, _) ->
  false.

%% clojerl.IError

message(#{?TYPE := ?M, message := Msg}) ->
  Msg.

%% clojerl.IExceptionInfo

data(#{?TYPE := ?M, data := Data}) ->
  Data.

cause(#{?TYPE := ?M, cause := Cause}) ->
  Cause.

%% clojerl.IHash

hash(#{?TYPE := ?M, data := Data}) ->
  erlang:phash2(Data).

%% clojerl.IStringable

str(#{?TYPE := ?M, message := Msg, data := Data}) ->
  DataBin = clj_rt:str(Data),
  clj_utils:error_str(?M, <<Msg/binary, " ", DataBin/binary>>).
