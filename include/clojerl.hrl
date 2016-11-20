%% Use a value that would be invalid for a keyword literal
-define(TYPE, '7ype').
-define(M, ?MODULE).
-define(CONSTRUCTOR, '__new__').
-define(UNBOUND, '__unbound__').

-record(?TYPE, { name = ?M  :: atom()
               , data       :: any()
               , info = #{} :: map()
               }).

%% nil
-define(NIL, undefined).
-define(NIL_TYPE, 'clojerl.Nil').

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
