-module('clojerl.List').

-export([
         new/1
        ]).

-include("include/clj_types.hrl").

-spec new(list()) -> 'list*'().
new(Items) -> {?MODULE, Items}.
