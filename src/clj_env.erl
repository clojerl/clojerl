-module(clj_env).

-export([empty_env/0]).

-type env() :: #{ns => atom()}.

-export_type([env/0]).

empty_env() -> #{ns => ?MODULE}.
