-module(clj_meta).

-export([attach/2]).

-include("include/clj_types.hrl").

-spec attach(meta(), sexpr()) -> sexpr().
attach(_Meta, Form) -> Form.
