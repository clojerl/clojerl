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
        Form     = clj_reader:read(InputBin, #{}, Env),
        %% Eval
        {Value, Env1} = clj_compiler:eval(Form, #{}, Env),
        %% Print
        io:format("~s~n", [clj_core:str(Value)]),
        %% Loop
        repl(Env1)
    catch
        _:Error ->
            io:format("~s~n", [clj_core:str(Error)]),
            repl(Env)
    end.
