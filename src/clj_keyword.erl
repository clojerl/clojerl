-module(clj_keyword).

-export([
         new/1,
         new/2
        ]).

new(Name) ->
  new(undefined, Name).

new(Namespace, Name) ->
  #{type => keyword,
    ns => Namespace,
    name => Name}.
