-module('clojerl.DummyResolver').

-include("clojerl.hrl").

-behavior('clojerl.IResolver').

-export([?CONSTRUCTOR/0]).
-export_type([type/0]).

-export([ 'current_ns'/1
        , 'resolve_class'/2
        , 'resolve_alias'/2
        , 'resolve_var'/2
        ]).

-type type() :: #{?TYPE => ?M}.

?CONSTRUCTOR() ->
  #{?TYPE => ?M}.

%%------------------------------------------------------------------------------
%% Protocols
%%------------------------------------------------------------------------------

current_ns(_) -> clj_rt:symbol(<<"dummy">>).

resolve_class(_, Symbol) -> Symbol.

resolve_alias(_, Symbol) -> Symbol.

resolve_var(_, Symbol) -> Symbol.
