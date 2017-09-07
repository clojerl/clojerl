%% nil
-define(NIL, undefined).
-define(NIL_TYPE, 'clojerl.Nil').

-define(NO_TAG, no_tag).
-define(RECUR_TAG, recur_tag).

%% Use a value that would be invalid for a keyword literal
-define(TYPE, '7ype').
-define(M, ?MODULE).
-define(CONSTRUCTOR, '__new__').
-define(UNBOUND, '__unbound__').

-define(IS_TYPE(X), (is_map(X) andalso maps:is_key(?TYPE, X))).

%% default type for protocol
-define(DEFAULT_TYPE, 'clojerl.Default').

%% Reader constants
-define(PLATFORM_FEATURES, [clje]).
-define(RESERVED_FEATURES, [else, none]).
-define(DEFAULT_FEATURE, default).

-define(OPT_EOF, eof).
-define(OPT_FEATURES, features).
-define(OPT_READ_COND, 'read-cond').
-define(OPT_IO_READER, 'io-reader').

-define(EOFTHROW, eofthrow).

%% Show this as a file when none is available
-define(NO_SOURCE, "NO_SOURCE_FILE").

%% Uncomment this define to enable debug messages.
%% -define(CLJ_DEBUG, true).

-ifdef(CLJ_DEBUG).
-define(DEBUG(Term), erlang:display(Term)).
-else.
-define(DEBUG(Term), ok).
-endif.

%% Internal

-define(CHUNK_SIZE, 32).
