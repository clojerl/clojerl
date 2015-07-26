-module(clj_compiler).

-export([is_special_symbol/1]).

-include("include/clj_types.hrl").

-define(SPECIALS, [clj_symbol:new('def'),
                   clj_symbol:new('loop*'),
                   clj_symbol:new('recur'),
                   clj_symbol:new('if'),
                   clj_symbol:new('case*'),
                   clj_symbol:new('let*'),
                   clj_symbol:new('letfn*'),
                   clj_symbol:new('do'),
                   clj_symbol:new('fn*'),
                   clj_symbol:new('quote'),
                   clj_symbol:new('var'),
                   clj_symbol:new('import*'),
                   clj_symbol:new('.'),
                   clj_symbol:new('set!'),
                   clj_symbol:new('deftype*'),
                   clj_symbol:new('reify*'),
                   clj_symbol:new('try'),
                   clj_symbol:new('throw'),
                   clj_symbol:new('monitor-enter'),
                   clj_symbol:new('monitor-exit'),
                   clj_symbol:new('catch'),
                   clj_symbol:new('finally'),
                   clj_symbol:new('new'),
                   clj_symbol:new('&')
                  ]).

-spec is_special_symbol(sexpr()) -> boolean().
is_special_symbol(s) ->
  lists:member(s, ?SPECIALS).
