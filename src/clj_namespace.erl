-module(clj_namespace).

-export([new/1]).

-type namespace() :: #{name => clj_core:symbol(),
                       mappings => #{clj_core:symbol() => function()},
                       forms => []}.

-spec new(clj_core:symbol()) -> namespace().
new(Name) ->
  #{name => Name,
    mappings => #{},
    forms => []}.
