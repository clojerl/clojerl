-module('clojerl.FakeVar').
-behavior('clojerl.IFn').

-include("clojerl.hrl").

-export([new/1]).
-export(['clojerl.IFn.invoke'/2]).

-type type() :: #?TYPE{}.

-spec new('clojerl.Var':type()) -> type().
new(Var) ->
  #?TYPE{data = Var}.

'clojerl.IFn.invoke'(#?TYPE{name =?M, data = Var}, Args) ->
  Module   = 'clojerl.Var':module(Var),
  Function = 'clojerl.Var':function(Var),

  Args1 = clj_core:seq_to_list(Args),
  Args2 = 'clojerl.Var':process_args(Var, Args1, fun clj_core:seq/1),

  Fun = clj_module:fake_fun(Module, Function, length(Args2)),

  erlang:apply(Fun, Args2).
