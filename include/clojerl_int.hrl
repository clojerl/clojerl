-ifndef(CLOJERL_INTERNAL).

-define(CLOJERL_INTERNAL, true).

-include("clojerl.hrl").

%%------------------------------------------------------------------------------
%% Compiler (analyzer/emitter)
%%------------------------------------------------------------------------------

%% nil
-define(NIL_TYPE, 'clojerl.Nil').

-define(NO_TAG, no_tag).
-define(RECUR_TAG, recur_tag).

%% default type for protocol
-define(DEFAULT_TYPE, 'clojerl.Default').

%% Name for the satisfies? function in protocols
-define(SATISFIES, '__satisfies?__').

%% Name for the extends? function in protocols
-define(EXTENDS, '__extends?__').

%% Name for the callback for behaviors
-define(BEHAVIOUR_INFO, behaviour_info).

%% Annotation used in detect emitted calls to locals functions
-define(LOCAL, local).

%% Environment to enable checking specs for macros
-define(CHECK_SPECS, 'clojure.core.check-specs').

%%------------------------------------------------------------------------------
%% Reader constants
%%------------------------------------------------------------------------------

-define(PLATFORM_FEATURES, [clje]).
-define(RESERVED_FEATURES, [else, none]).
-define(DEFAULT_FEATURE, default).

-define(OPT_EOF, eof).
-define(OPT_FEATURES, features).
-define(OPT_READ_COND, 'read-cond').
-define(OPT_IO_READER, 'io-reader').

-define(EOFTHROW, eofthrow).

-define(INT_PATTERN,
        "^([-+]?)"
        "(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|"
        "([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?$").
-define(FLOAT_PATTERN, "^(([-+]?[0-9]+)(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$").

%% Show this as a file when none is available
-define(NO_SOURCE, "NO_SOURCE_FILE").

%%------------------------------------------------------------------------------
%% Debug
%%------------------------------------------------------------------------------

%% Uncomment this define to enable debug messages.
%% -define(CLJ_DEBUG, true).

-ifdef(CLJ_DEBUG).
-define(DEBUG(Term), io:format("~s~n~n", [clj_rt:str(Term)])).
-define( DEBUG_WHEN(Pred, Term)
       , case Pred of
           true  -> ?DEBUG(Term);
           false -> ok
         end
       ).
-else.
-define(DEBUG(Term), ok).
-define(DEBUG_WHEN(Pred, Term), ok).
-endif.

%%------------------------------------------------------------------------------
%% ETS Tables
%%------------------------------------------------------------------------------

-define(AGENT_TABLE, 'clojerl.Agent').

%%------------------------------------------------------------------------------
%% Chunk size
%%------------------------------------------------------------------------------

-define(CHUNK_SIZE, 32).

%%------------------------------------------------------------------------------
%% Errors and warnings
%%------------------------------------------------------------------------------

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
-define( ERROR(Msg, Loc, Stacktrace)
       , erlang:raise(
           error,
           'clojerl.Error':?CONSTRUCTOR(clj_utils:format_error(Msg, Loc)),
           Stacktrace
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

-ifdef(FUN_STACKTRACE).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.

-endif.
