-module(clj_edn).

-include("clojerl.hrl").
-include("clojerl_int.hrl").

-export([read/2, read_string/2]).

-type location() :: #{ line   => non_neg_integer()
                     , column => non_neg_integer()
                     , file   => binary() | ?NIL
                     }.

-define(READERS, readers).
-define(DEFAULT, default).

-type opts() :: #{ ?OPT_EOF       => ?EOFTHROW | ok
                   %% When 'eofthrow' then throw, otherwise return value.
                 , file           => file:filename_all()
                   %% Source file being read.
                 , ?OPT_IO_READER => 'erlang.io.IPushbackReader':type()
                   %% IReader that should be used when there are no more
                   %% characters to be read from the binary.
                 , ?READERS       => #{} | ?NIL
                 }.

-export_type([location/0, opts/0]).

-type state() :: #{ src           => binary()
                    %% A binary the represents Clojure source code
                  , opts          => opts()
                    %% The options map supplied to the reader.
                  , forms         => [any()]
                    %% List of forms read (in reverse order).
                  , loc           => {non_neg_integer(), non_neg_integer()}
                    %% Current line and column location.
                  , bindings      => clj_scope:scope()
                    %% Current bindings.
                  , return_on     => char() | ?NIL
                    %% Used when reading a string
                  , current       => binary() | ?NIL
                    %% Compiled regexes for numbers
                  , number_types  => [{clj_utils:number_type(), re:mp()}]
                  }.

%% @doc Reads the next form from the input. Returns the form
%%      or throws if there is no form to read.
-spec read(binary(), opts()) -> any().
read(Src, Opts) ->
  State = new_state(Src, Opts),
  try
    ensure_read(State)
  catch
    throw:{eof, Value, _} -> Value
  end.

-spec read_string(binary(), opts()) -> any().
read_string(Src, Opts) ->
  read(Src, Opts).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
-spec new_state(binary(), opts()) -> state().
new_state(Src, Opts) ->
  {ok, IntRegex}   = re:compile(?INT_PATTERN),
  {ok, FloatRegex} = re:compile(?FLOAT_PATTERN),
  #{ src           => Src
   , opts          => Opts
   , forms         => []
   , loc           => {1, 1}
   , bindings      => clj_scope:new()
   , return_on     => ?NIL
   , current       => ?NIL
   , number_types  => [{int, IntRegex}, {float, FloatRegex}]
   }.

%% @doc Makes sure a single form is read unless we reach
%%      the enf of file. It handles the case were an ignore
%%      reader is used.
%% @private
-spec ensure_read(state()) -> any().
ensure_read(#{forms := [Form]}) ->
  Form;
ensure_read(State) ->
  ensure_read(read_one(State)).

-spec read_one(state()) -> state().
read_one(#{src := <<>>, opts := Opts} = State) ->
  Eof = maps:get(?OPT_EOF, Opts, ?EOFTHROW),
  %% If we got here it's because we were expecting something
  %% and it wasn't there.
  case check_reader(State) of
    {ok, NewState} ->
      read_one(NewState);
    eof when Eof =:= ?EOFTHROW ->
      ?ERROR(<<"EOF">>, location(State));
    eof ->
      throw({eof, Eof, State})
  end;
read_one(#{src := <<Ch/utf8, _/binary>>, return_on := Ch} = State) ->
  throw({return_on, consume_char(State)});
read_one(#{src := <<First/utf8, Rest/binary>>} = State) ->
  Second = peek_src(State#{src := Rest}),
  case clj_utils:char_type(First, Second) of
    whitespace      -> read_one(consume_char(State));
    number          -> read_number(State);
    string          -> read_string(State);
    keyword         -> read_keyword(State);
    comment         -> read_comment(State);
    meta            -> read_meta(State);
    list            -> read_list(State);
    vector          -> read_vector(State);
    map             -> read_map(State);
    unmatched_delim -> read_unmatched_delim(State);
    char            -> read_char(State);
    dispatch        -> read_dispatch(State);
    symbol          -> read_symbol(State)
  end.

%%------------------------------------------------------------------------------
%% Numbers
%%------------------------------------------------------------------------------

-spec read_number(state()) -> state().
read_number(#{number_types := Types} = State0) ->
  {Current, State1} = consume(State0, [number, symbol]),
  Number = clj_utils:parse_number(Current, Types),
  push_form(Number, State1).

%%------------------------------------------------------------------------------
%% String
%%------------------------------------------------------------------------------

-spec read_string(state()) -> state().
%% Start consuming string
read_string(#{ src := <<"\"", _/binary>>
             , loc := Loc
             , current := ?NIL
             } = State
           ) ->
  State1 = add_scope(consume_char(State)),
  read_string(location_started(State1#{current => <<>>}, Loc));
%% Found closing double quotes
read_string(#{ src     := <<"\"", _/binary>>
             , current := String
             } = State0
           ) ->
  State = consume_char(State0#{current => ?NIL}),
  push_form(String, remove_scope(State));
%% Process escaped character
read_string(#{ src     := <<"\\", _/binary>>
             , current := String
             } = State0
           ) ->
  {EscapedChar, State00} = escape_char(consume_char(State0)),
  State = State00#{current => <<String/binary, EscapedChar/binary>>},

  read_string(State);
%% Append character to current string
read_string(#{ src     := <<Char/utf8, _/binary>>
             , current := String} = State0) ->
  State = State0#{current => <<String/binary, Char/utf8>>},
  read_string(consume_char(State));
%% Didn't find closing double quotes
read_string(#{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState}  ->
      read_string(NewState);
    eof ->
      {Line, Col} = scope_get(loc_started, State),
      ?ERROR( [ <<"Started reading at (">>
              , Line, <<":">>, Col
              , <<") but found EOF while reading string">>
              ]
            , location(State)
            )
  end.

-spec escape_char(state()) -> {binary(), state()}.
escape_char(State = #{src := <<>>}) ->
  case check_reader(State) of
    eof -> ?ERROR(<<"EOF while escaping char">>, location(State));
    {ok, NewState} -> escape_char(NewState)
  end;
escape_char(State = #{src := <<Char/utf8, _/binary>>}) ->
  CharType = clj_utils:char_type(Char, peek_src(State)),
  case Char of
    $t  -> {<<"\t">>, consume_char(State)};
    $r  -> {<<"\r">>, consume_char(State)};
    $n  -> {<<"\n">>, consume_char(State)};
    $\\ -> {<<"\\">>, consume_char(State)};
    $"  -> {<<"\"">>, consume_char(State)};
    $b  -> {<<"\b">>, consume_char(State)};
    $f  -> {<<"\f">>, consume_char(State)};
    $u  ->
      %% Hexa unicode
      State1   = consume_char(State),
      NextChar = peek_src(State1),
      ?ERROR_WHEN( NextChar =/= eof
                   andalso not is_char_valid(NextChar, 16)
                 , [ <<"Invalid unicode escape: \\u">>
                   , NextChar =/= eof andalso <<NextChar/utf8>>
                   ]
                 , location(State1)
                 ),
      {CodePoint, State2} = unicode_char(State1, 16, 4, true),
      {unicode:characters_to_binary([CodePoint], utf8), State2};
    _ when CharType == number ->
      %% Octal unicode
      {CodePoint, State1} = unicode_char(State, 8, 3, false),
      ?ERROR_WHEN( CodePoint > 8#377
                 , <<"Octal escape sequence must be in "
                     "range [0, 377]">>
                 , location(State1)
                 ),
      {unicode:characters_to_binary([CodePoint], utf8), State1};
    _ ->
      ?ERROR( <<"Unsupported escape character: \\", Char>>
            , location(State)
            )
  end.

-spec unicode_char(state(), integer(), integer(), Exact :: boolean()) ->
  {integer(), state()}.
unicode_char(State, Base, Length, IsExact) ->
  {Number, State1} = read_token(State, Length),
  Size = case IsExact of
           true -> size(Number);
           false -> Length
         end,

  ?ERROR_WHEN( Size =/= Length
             , [ <<"Invalid character length: ">>, Size
               , <<", should be: ">>, Length
               ]
             , location(State)
             ),
  Invalid      = fun(C) -> not is_char_valid(C, Base) end,
  Chars        = unicode:characters_to_list(Number),
  InvalidChars = lists:filter(Invalid, Chars),
  InvalidChar  = case InvalidChars of
                   [] -> ?NIL;
                   [C | _] -> <<C/utf8>>
                 end,
  ?ERROR_WHEN( InvalidChars =/= []
             , [ <<"Invalid digit: ">>, InvalidChar]
             , location(State)
             ),

  NumberInt = binary_to_integer(Number, Base),
  ?ERROR_WHEN(  16#D800 =< NumberInt andalso NumberInt =< 16#DFFF
             , [ <<"Invalid UTF-8 character number: \\u">>, Number]
             , location(State)
             ),

  {NumberInt, State1}.

-spec is_char_valid(integer(), integer()) -> boolean().
is_char_valid(C, Base) when C >= $0, C =< $9 -> C - $0 + 1  =< Base;
is_char_valid(C, Base) when C >= $a, C =< $z -> C - $a + 11 =< Base;
is_char_valid(C, Base) when C >= $A, C =< $Z -> C - $A + 11 =< Base;
is_char_valid(_, _) -> false.


%%------------------------------------------------------------------------------
%% Keyword
%%------------------------------------------------------------------------------

-spec read_keyword(state()) -> state().
read_keyword(#{src := <<":", _/binary>>} = State0) ->
  {Token, State} = read_token(consume_char(State0)),
  Keyword =
    case clj_utils:parse_symbol(Token) of
      {?NIL, <<":", Name/binary>>} ->
        Ns    = 'clojerl.Namespace':current(),
        NsSym = 'clojerl.Namespace':name(Ns),
        Namespace = 'clojerl.Symbol':name(NsSym),
        clj_rt:keyword(Namespace, Name);
      {?NIL, Name} ->
        clj_rt:keyword(Name);
      {<<":", Namespace/binary>>, Name} ->
        AliasSym  = clj_rt:symbol(Namespace),
        CurrentNs = 'clojerl.Namespace':current(),
        case 'clojerl.Namespace':alias(CurrentNs, AliasSym) of
          ?NIL -> ?ERROR(<<"Invalid token: :", Token/binary>>, location(State));
          Ns ->
            NsSym = 'clojerl.Namespace':name(Ns),
            clj_rt:keyword('clojerl.Symbol':name(NsSym), Name)
        end;
      {Namespace, Name} ->
        clj_rt:keyword(Namespace, Name);
      _ ->
        ?ERROR(<<"Invalid token: :", Token/binary>>, location(State))
    end,
  push_form(Keyword, State).

%%------------------------------------------------------------------------------
%% Symbol
%%------------------------------------------------------------------------------

-spec read_symbol(state()) -> state().
read_symbol(State) ->
  {Token, State1} = read_token(State),
  Symbol = case clj_utils:parse_symbol(Token) of
             {?NIL, <<"nil">>}   -> ?NIL;
             {?NIL, <<"true">>}  -> true;
             {?NIL, <<"false">>} -> false;
             {?NIL, Name} ->
               clj_rt:symbol(Name);
             {Ns, Name} ->
               clj_rt:symbol(Ns, Name);
             ?NIL ->
               ?ERROR(<<"Invalid token: ", Token/binary>>, location(State))
           end,

  push_form(Symbol, State1).

%%------------------------------------------------------------------------------
%% Comment
%%------------------------------------------------------------------------------

-spec read_comment(state()) -> state().
read_comment(State0) ->
  read_one(skip_line(State0)).

%%------------------------------------------------------------------------------
%% Meta
%%------------------------------------------------------------------------------

-spec read_meta(state()) -> state().
read_meta(#{src := <<"^"/utf8, Src/binary>>} = State) ->
  {SugaredMeta, State1} = read_pop_one(State#{src => Src}),
  Meta = clj_utils:desugar_meta(SugaredMeta),

  {Form, State2} = read_pop_one(State1),
  FormMeta = clj_rt:merge([clj_rt:meta(Form), Meta]),
  NewMeta  = case clj_rt:'seq?'(Form) of
              true  -> clj_rt:merge([FormMeta, file_location_meta(State)]);
              false -> FormMeta
            end,

  NewForm = clj_rt:with_meta(Form, NewMeta),

  push_form(NewForm, State2).

%%------------------------------------------------------------------------------
%% List
%%------------------------------------------------------------------------------

-spec read_list(state()) -> state().
read_list(#{ src   := <<"("/utf8, _/binary>>
           , forms := Forms
           , loc   := Loc
           } = State0
         ) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($), location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items = lists:reverse(ReversedItems),
  List = 'clojerl.List':with_meta( clj_rt:list(Items)
                                 , file_location_meta(State0)
                                 ),

  State2#{forms => [List | Forms]}.

%%------------------------------------------------------------------------------
%% Vector
%%------------------------------------------------------------------------------

-spec read_vector(state()) -> state().
read_vector(#{ src   := <<"["/utf8, _/binary>>
             , forms := Forms
             , loc   := Loc
             } = State0
           ) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($], location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items = lists:reverse(ReversedItems),
  Vector = 'clojerl.Vector':with_meta( clj_rt:vector(Items)
                                     , file_location_meta(State0)
                                     ),

  State2#{forms => [Vector | Forms]}.

%%------------------------------------------------------------------------------
%% Map
%%------------------------------------------------------------------------------

-spec read_map(state()) -> state().
read_map(#{ src   := <<"{"/utf8, _/binary>>
          , forms := Forms
          , loc   := Loc
          } = State0
        ) ->
  State  = add_scope(consume_char(State0)),
  State1 = read_until($}, location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  case length(ReversedItems) of
    X when X rem 2 == 0 ->
      Items = lists:reverse(ReversedItems),
      Map = 'clojerl.Map':with_meta( clj_rt:hash_map(Items)
                                   , file_location_meta(State0)
                                   ),
      State2#{forms => [Map | Forms]};
    _ ->
      ?ERROR( <<"Map literal must contain an even number of forms">>
            , location(State2)
            )
  end.

%%------------------------------------------------------------------------------
%% Unmatched delimiter
%%------------------------------------------------------------------------------

-spec read_unmatched_delim(state()) -> no_return().
read_unmatched_delim(#{src := <<Delim/utf8, _/binary>>} = State) ->
  ?ERROR(<<"Umatched delimiter ", Delim/utf8>>, location(State)).

%%------------------------------------------------------------------------------
%% Character
%%------------------------------------------------------------------------------

-spec read_char(state()) -> state().
read_char(#{src := <<"\\"/utf8, _/binary>>} = State0) ->
  State    = consume_char(State0),
  NextChar = case peek_src(State) of
               eof -> ?ERROR( <<"EOF while reading character">>
                            , location(State)
                            );
               NextCh  -> NextCh
             end,
  {Token, State1} =
    case is_macro_terminating(NextChar) orelse is_whitespace(NextChar) of
      true -> {<<NextChar/utf8>>, consume_char(State)};
      false -> read_token(State)
    end,
  Char =
    case Token of
      <<_/utf8>> -> Token;
      <<"newline">> -> $\n;
      <<"space">> -> $ ;
      <<"tab">> -> $\t;
      <<"backspace">> -> $\b;
      <<"formfeed">> -> $\f;
      <<"return">> -> $\r;
      <<"u", RestToken/binary>> ->
        TokenLength = 'clojerl.String':count(RestToken),
        ?ERROR_WHEN( TokenLength =/= 4
                   , [ <<"Invalid unicode character: \\u">>
                     , RestToken
                     ]
                   , location(State1)
                   ),
        {Ch, _} = unicode_char(State1#{src => RestToken}, 16, 4, true),
        Ch;
      <<"o", RestToken/binary>> ->
        read_octal_char(State1#{src => RestToken});
      Ch -> ?ERROR( <<"Unsupported character: \\", Ch/binary>>
                  , location(State)
                  )
    end,

  CharBin = unicode:characters_to_binary([Char]),
  push_form(CharBin, State1).

-spec read_octal_char(state()) -> char().
read_octal_char(#{src := RestToken} = State) when size(RestToken) > 3 ->
  Size    = size(RestToken),
  SizeBin = integer_to_binary(Size),
  ?ERROR( <<"Invalid octal escape sequence length: ", SizeBin/binary>>
        , location(State)
        );
read_octal_char(#{src := RestToken} = State) ->
  Size = size(RestToken),
  case unicode_char(State, 8, Size, true) of
    {Ch, _} when Ch > 8#377 ->
      ?ERROR( <<"Octal escape sequence must be in range [0, 377]">>
            , location(State)
            );
    {Ch, _} -> Ch
  end.

%%------------------------------------------------------------------------------
%% Reader dispatch
%%------------------------------------------------------------------------------

-spec read_dispatch(state()) -> state().
read_dispatch(#{src := <<"#">>} = State) ->
  case check_reader(State#{src := <<>>}) of
    {ok, NewState = #{src := NewSrc}}  ->
      read_dispatch(NewState#{src := <<"#", NewSrc/binary>>});
    eof ->
      ?ERROR(<<"EOF while reading dispatch">>, location(State))
  end;
read_dispatch(#{src := <<"#"/utf8, Src/binary>>} = State) ->
  <<Ch/utf8, _/binary>> = Src,
  NewState = consume_chars(2, State),
  case Ch of
    $^ -> read_meta(consume_char(State)); %% deprecated
    ${ -> read_set(NewState);
    $_ -> read_discard(NewState);
    $< -> ?ERROR(<<"Unreadable form">>, location(State));
    _  ->
      case clj_utils:char_type(Ch, ?NIL) of
        symbol -> read_tagged(consume_char(State));
        _      -> ?ERROR(<<"No dispatch macro for: ", Ch/utf8>>, ?NIL)
      end
  end.

%%------------------------------------------------------------------------------
%% #{} set
%%------------------------------------------------------------------------------

-spec read_set(state()) -> state().
read_set(#{forms := Forms, loc := Loc} = State0) ->
  State  = add_scope(State0),
  State1 = read_until($}, location_started(State#{forms => []}, Loc)),
  State2 = remove_scope(State1),
  #{forms := ReversedItems} = State2,

  Items       = lists:reverse(ReversedItems),
  Set         = clj_rt:hash_set(Items),
  SetWithMeta = 'clojerl.Set':with_meta(Set, file_location_meta(State0)),

  State2#{forms => [SetWithMeta | Forms]}.

%%------------------------------------------------------------------------------
%% #_ discard
%%------------------------------------------------------------------------------

-spec read_discard(state()) -> state().
read_discard(#{forms := Forms, return_on := ReturnOn} = State0) ->
  %% Remove forms and return_on to avoid discarding something unintentionally.
  State1 = State0#{forms := [], return_on := ?NIL},
  {_, State2} = read_pop_one(State1),
  %% Can't call read_one here because is might not be a top level form.
  State2#{forms := Forms, return_on := ReturnOn}.

%%------------------------------------------------------------------------------
%% # reader tag
%%------------------------------------------------------------------------------

-spec read_tagged(state()) -> state().
read_tagged(State) ->
  {Symbol, State1} = read_pop_one(State),

  ?ERROR_WHEN( not clj_rt:'symbol?'(Symbol)
             , <<"Reader tag must be a symbol">>
             , location(State)
             ),

  {Form, State2} = read_pop_one(State1),
  read_tagged(Symbol, Form, location(State), State2).

-spec read_tagged('clojerl.Symbol':type(), any(), any(), state()) ->
  state().
read_tagged(Symbol, Form, Location, #{opts := Opts} = State) ->
  DefaultDataReadersVar = 'clojerl.Var':?CONSTRUCTOR( <<"clojure.core">>
                                                    , <<"default-data-readers">>
                                                    ),
  DefaultDataReaders    = 'clojerl.Var':deref(DefaultDataReadersVar),

  DataReaders = clj_rt:get(Opts, ?READERS),
  Reader0     = clj_rt:get(DataReaders, Symbol),
  Reader1     = case Reader0 of
                  ?NIL -> clj_rt:get(DefaultDataReaders, Symbol);
                  true -> Reader0
                end,

  { IsDefault
  , Reader2
  } = case Reader1 of
        ?NIL -> {true,  clj_rt:get(DefaultDataReaders, ?DEFAULT)};
        _    -> {false, Reader1}
      end,

  ?ERROR_WHEN( Reader2 =:= ?NIL
             , [<<"No reader function for tag ">>, Symbol]
             , Location
             ),

  ReadForm = case IsDefault of
               true  -> clj_rt:apply(Reader2, [Symbol, Form]);
               false -> clj_rt:apply(Reader2, [Form])
             end,
  push_form(ReadForm, State).

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec location(state()) -> location().
location(#{opts := Opts} = State) ->
  {Line, Col} = case maps:get(loc, State, ?NIL) of
                  ?NIL -> {?NIL, ?NIL};
                  Loc  -> Loc
                end,
  #{ line   => Line
   , column => Col
   , file   => maps:get(file, Opts, ?NIL)
   }.

-spec location_started(state(), {non_neg_integer(), non_neg_integer()}) ->
  state().
location_started(State, Loc) ->
  scope_put(loc_started, Loc, State).

-spec consume_char(state()) -> state().
consume_char(#{src := <<"\n"/utf8, Src/binary>>, loc := {Line, _}} = State) ->
  State#{src => Src, loc => {Line + 1, 1}};
consume_char(#{src := <<_/utf8, Src/binary>>, loc := {Line, Col}} = State) ->
  State#{src => Src, loc => {Line, Col + 1}};
consume_char(State) ->
  case check_reader(State) of
    {ok, NewState} -> consume_char(NewState);
    eof -> State
  end.

-spec consume_chars(non_neg_integer(), state()) -> state().
consume_chars(0, State) ->
  State;
consume_chars(N, State) when N > 0 ->
  consume_chars(N - 1, consume_char(State)).

-spec consume(state(), [clj_utils:char_type()] | fun()) ->
  {binary(), state()}.
consume(State, TypesOrPred) ->
  consume(State, TypesOrPred, -1).

-spec consume(state(), [clj_utils:char_type()] | fun(), integer()) ->
  {binary(), state()}.
consume(State, TypesOrPred, Length) ->
  do_consume(State, <<>>, TypesOrPred, Length).

-spec do_consume( state()
                , binary()
                , [clj_utils:char_type()] | fun()
                , integer()
                ) ->
  {binary(), state()}.
do_consume(State, Acc, _TypesOrPred, 0) ->
  {Acc, State};
do_consume(State = #{src := <<>>}, Acc, TypesOrPred, Length) ->
  case check_reader(State) of
    {ok, NewState} -> do_consume(NewState, Acc, TypesOrPred, Length);
    eof -> {Acc, State}
  end;
do_consume( State = #{src := <<X/utf8, _/binary>>}
          , Acc
          , Pred
          , Length
          ) when is_function(Pred) ->
  case Pred(X) of
    true  ->
      State1 = consume_char(State),
      do_consume(State1, <<Acc/binary, X/utf8>>, Pred, Length - 1);
    false -> {Acc, State}
  end;
do_consume( State = #{src := <<X/utf8, Rest/binary>>}
          , Acc
          , Types
          , Length
          ) ->
  Next = case Rest of
           <<N/utf8, _>> -> N;
           _             -> -1
         end,
  Type = clj_utils:char_type(X, Next),
  case lists:member(Type, Types) of
    true ->
      State1 = consume_char(State),
      do_consume(State1, <<Acc/binary, X/utf8>>, Types, Length - 1);
    false ->
      {Acc, State}
  end.

-spec read_token(state()) -> {binary(), state()}.
read_token(State) ->
  read_token(State, -1).

-spec read_token(state(), integer()) -> {binary(), state()}.
read_token(State, Length) ->
  Fun = fun(Char) ->
            (not is_whitespace(Char))
              andalso (not is_macro_terminating(Char))
        end,
  consume(State, Fun, Length).

-spec read_until(char(), state()) -> state().
read_until(Delim, State) ->
  read_until(Delim, fun read_one/1, State).

-spec read_until(char(), function(), state()) -> state().
read_until(Delim, ReadFun, #{src := <<>>} = State) ->
  case check_reader(State) of
    {ok, NewState} ->
      read_until(Delim, ReadFun, NewState);
    eof ->
      {Line, Col} = scope_get(loc_started, State),
      ?ERROR( [ <<"Started reading at (">>
              , Line, <<":">>, Col
              , <<") but found EOF while expecting '">>
              , <<Delim/utf8>>
              , <<"'">>
              ]
            , location(State)
            )
  end;
read_until(Delim, ReadFun, State) ->
  State1 = scope_put(read_delim, true, State#{return_on => Delim}),
  try
    read_until(Delim, ReadFun, ReadFun(State1))
  catch throw:{return_on, State2} ->
      finish_read_until(State2)
  end.

-spec finish_read_until(state()) -> state().
finish_read_until(State0) ->
  State1 = State0#{return_on := ?NIL},
  scope_put(read_delim, false, State1).

-spec is_macro_terminating(char()) -> boolean().
is_macro_terminating(Char) ->
  lists:member(Char,
               [$", $;, $@, $^, $`, $~, $(,
                $), $[, $], ${, $}, $\\ ]).

-spec is_whitespace(char()) -> boolean().
is_whitespace(Char) ->
  clj_utils:char_type(Char) =:= whitespace.

-spec skip_line(state()) -> state().
skip_line(State) ->
  NotNewline = fun(C) -> C =/= $\n andalso C =/= $\r end,
  {_, State1} = consume(State, NotNewline),
  State1.

-spec read_pop_one(state()) -> {any(), state()}.
read_pop_one(State0) ->
  case read_one(State0) of
    #{forms := [Form | Forms]} = State1 ->
      {Form, State1#{forms => Forms}};
    State1 ->
      read_pop_one(State1)
  end.

-spec push_form(any(), state()) -> state().
push_form(Form, #{forms := Forms} = State) ->
  State#{forms => [Form | Forms]}.

-spec scope_get(atom(), state()) -> any().
scope_get(Name, #{bindings := Bindings} = _State) ->
  clj_scope:get(Name, Bindings).

-spec scope_put(atom(), any(), state()) -> state().
scope_put(Name, Value, #{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:put(Name, Value, Bindings)}.

-spec add_scope(state()) -> state().
add_scope(#{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:new(Bindings)}.

-spec remove_scope(state()) -> state().
remove_scope(#{bindings := Bindings} = State) ->
  State#{bindings => clj_scope:parent(Bindings)}.

-spec file_location_meta(state()) -> map().
file_location_meta(#{loc := {Line, Col}, opts := Opts}) ->
  #{ line   => Line
   , column => Col
   , file   => maps:get(file, Opts, ?NIL)
   }.

-spec peek_src(state()) -> integer() | eof.
peek_src(#{src := <<First/utf8, _/binary>>}) ->
  First;
peek_src(#{src := <<>>, opts := #{?OPT_IO_READER := Reader}}) ->
  case 'erlang.io.IReader':read(Reader) of
    eof -> eof;
    <<Ch/utf8>> = ChBin ->
      'erlang.io.IPushbackReader':unread(Reader, ChBin),
      Ch
  end;
peek_src(_State) ->
  eof.

-spec check_reader(state()) -> {ok, state()} | eof.
check_reader(#{src := <<>>, opts := #{?OPT_IO_READER := Reader}} = State)
  when Reader =/= ?NIL ->
  case 'erlang.io.IReader':read(Reader) of
    eof -> eof;
    Ch  -> {ok, State#{src := Ch}}
  end;
check_reader(#{src := <<>>}) ->
  eof.
