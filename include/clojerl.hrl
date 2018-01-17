%% nil
-define(NIL, undefined).

%% Type
-define(TYPE, '__type__').
-define(M, ?MODULE).
-define(CONSTRUCTOR, '__new__').
-define(UNBOUND, '__unbound__').

-define(IS_TYPE(X), (is_map(X) andalso maps:is_key(?TYPE, X))).
-define(MATCH_TYPE, #{?TYPE := _}).
-define(MATCH_TYPE(X), #{?TYPE := X}).
