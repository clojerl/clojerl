-module(clj_repl).

-export([repl/1]).

-spec repl(clj_env:env()) -> clj_env:env().
repl(Env) ->
  try
    CurrentNsSym = clj_env:current_ns(Env),
    CurrentNsBin = clj_core:str(CurrentNsSym),
    PromptBin    = <<CurrentNsBin/binary, "=> ">>,

    %% Read
    Input    = io:get_line(binary_to_list(PromptBin)),
    InputBin = list_to_binary(Input),

    {Output, Env1} =
      case skip_whitespace(InputBin) of
        'request-prompt' ->
          {"", Env};
        _ ->
          Form = clj_reader:read(InputBin, #{}, Env),
          %% Eval
          {Value, EnvTemp} = clj_compiler:eval(Form, #{}, Env),
          {clj_core:str(Value), EnvTemp}
      end,

    %% Print
    io:format("~s~n", [Output]),
    %% Loop
    repl(Env1)
  catch
    _:Error ->
      io:format("~s~n", [clj_core:str(Error)]),
      io:format("~p~n", [erlang:get_stacktrace()]),
      repl(Env)
  end.

-spec skip_whitespace(binary()) -> 'request-prompt' | binary().
skip_whitespace(<<>>) ->
  'request-prompt';
skip_whitespace(<<Ch/utf8, Rest/binary>> = Input) ->
  case clj_utils:char_type(Ch) of
    whitespace -> skip_whitespace(Rest);
    _ -> Input
  end.
