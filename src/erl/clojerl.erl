%% @doc Clojerl API.
%%
%% User-friendly API to interact with Clojerl from other BEAM
%% languages.
-module(clojerl).

-include("clojerl_int.hrl").

-export([ start/0
        , read/1
        , var/1
        , var/2
        ]).

-dialyzer({nowarn_function, read/1}).

%% @doc Starts the `clojerl' OTP application.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(clojerl, permanent),
  ok.

%% @doc Reads a binary string and returns a parsed Clojerl form.
-spec read(binary()) -> any().
read(Str) ->
  'clojure.edn':'read-string'(Str).

%% @doc Returns the var described by `QualifiedName'.
-spec var(binary() | 'clojerl.Symbol':type()) -> 'clojerl.Var':type().
var(QualifiedName) ->
  QualifiedSymbol = as_symbol(QualifiedName),
  'clojerl.Var':find(QualifiedSymbol).

%% @doc Returns the `Name' var in namespace `Ns'.
-spec var(any(), any()) -> 'clojerl.Var':type().
var(Ns, Name) ->
  NsStr   = 'clojerl.Symbol':str(as_symbol(Ns)),
  NameStr = 'clojerl.Symbol':str(as_symbol(Name)),
  QualifiedSymbol = clj_rt:symbol(NsStr, NameStr),
  'clojerl.Var':find(QualifiedSymbol).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec as_symbol(binary() | 'clojerl.Symbol':type()) ->
  'clojerl.Symbol':type().
as_symbol(X) when is_binary(X) ->
  'clojerl.Symbol':?CONSTRUCTOR(X);
as_symbol(X) ->
  ?ERROR_WHEN(not clj_rt:'symbol?'(X), [<<"Invalid symbol ">>, X]),
  X.
