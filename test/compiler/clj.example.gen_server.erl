-module('clj.example.gen_server').

-behavior(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

init(Args) ->
  VarFun = 'clj.example.gen_server.init__var':val(),
  VarFun([Args]).

handle_call(Req, From, State) ->
  VarFun = 'clj.example.gen_server.handle_call__var':val(),
  VarFun([Req, From, State]).

handle_cast(Req, State) ->
  VarFun = 'clj.example.gen_server.handle_cast__var':val(),
  VarFun([Req, State]).

handle_info(Info, State) ->
  VarFun = 'clj.example.gen_server.handle_info__var':val(),
  VarFun([Info, State]).

terminate(Reason, State) ->
  VarFun = 'clj.example.gen_server.terminate__var':val(),
  VarFun([Reason, State]).

code_change(OldVsn, State, Extra) ->
  VarFun = 'clj.example.gen_server.code_change__var':val(),
  VarFun([OldVsn, State, Extra]).

format_status(Opt, [PDict, State]) ->
  VarFun = 'clj.example.gen_server.format_status__var':val(),
  VarFun([Opt, [PDict, State]]).
