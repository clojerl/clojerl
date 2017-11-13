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
-define(DEBUG(Term), io:format("~p~n", [Term])).
-else.
-define(DEBUG(Term), ok).
-endif.

%% Chunk size

-define(CHUNK_SIZE, 32).

%% Errors and warnings

-define(THROW(Msg), ?THROW(Msg, ?NIL)).
-define(THROW(Msg, Loc), erlang:throw(clj_utils:format_error(Msg, Loc))).

-define(THROW_WHEN(Pred, Msg), ?THROW_WHEN(Pred, Msg, ?NIL)).
-define( THROW_WHEN(Pred, Msg, Loc)
       , case Pred of
           true  -> ?THROW(Msg, Loc);
           false -> ok
         end
       ).

-define(ERROR(Msg), ?ERROR(Msg, ?NIL)).
-define( ERROR(Msg, Loc)
       , erlang:error(
           'clojerl.Error':?CONSTRUCTOR(clj_utils:format_error(Msg, Loc))
          )
       ).

-define(ERROR_WHEN(Pred, Msg), ?ERROR_WHEN(Pred, Msg, ?NIL)).
-define( ERROR_WHEN(Pred, Msg, Loc)
       , case Pred of
           true  -> ?ERROR(Msg, Loc);
           false -> ok
         end
       ).

-define(WARN(Msg), ?WARN(Msg, ?NIL)).
-define( WARN(Msg, Loc)
       , 'erlang.io.IWriter':write(
           'clojure.core':'*err*__val'(),
           <<(clj_utils:format_error(Msg, Loc))/binary, "\n">>
          )
       ).

-define(WARN_WHEN(Pred, Msg), ?WARN_WHEN(Pred, Msg, ?NIL)).
-define( WARN_WHEN(Pred, Msg, Loc)
       , case Pred of
           true  -> ?WARN(Msg, Loc);
           false -> ok
         end
       ).

-endif.
