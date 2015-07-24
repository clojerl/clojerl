-module(clj_symbol).

-export([
         new/1,
         new/2
        ]).

new(Name) ->
  new(undefined, Name).

new(Namespace, Name) ->
  #{type => symbol,
    ns => Namespace,
    name => Name}.
