-ifndef(NIL_TYPE).

%% nil
-define(NIL_TYPE, 'clojerl.Nil').

-define(NO_TAG, no_tag).
-define(RECUR_TAG, recur_tag).

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

%% Chunk size

-define(CHUNK_SIZE, 32).

-endif.
