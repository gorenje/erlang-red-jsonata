-module(erlang_red_jsonata).

-export([
    evaluate_erlang/1,
    execute/2,
    jsonata_to_erlang/1
]).

%%
%% The UberAPI to the jsonata_leex and jsonata_parser modules.
%%
%% This module provides a single interface for evaluating a JSONata stanza
%% in the presence of the Msg object.
%%

execute(JSONata, Msg) when is_binary(JSONata) ->
    execute(binary_to_list(JSONata), Msg);
execute(JSONata, Msg) ->
    try
        case jsonata_to_erlang(JSONata) of
            {ok, ErlangCode} ->
                case evaluate_erlang(binary_to_list(ErlangCode)) of
                    {ok, Func} ->
                        {ok, Func(Msg)};
                    Error ->
                        ErrMsg = io_lib:format(
                            "Stanza: {{{ ~p }}} Error: ~p",
                            [ErlangCode, Error]
                        ),
                        {error, ErrMsg}
                end;
            Error ->
                {error, Error}
        end
    catch
        error:jsonata_unsupported:Stacktrace ->
            [H | _] = Stacktrace,
            {exception,
                list_to_binary(
                    io_lib:format(
                        "jsonata unsupported function: ~p", element(3, H)
                    )
                )}
    end.

%%
%% Inspired by this blog post:
%% https://grantwinney.com/how-to-evaluate-a-string-of-code-in-erlang-at-runtime/
%%
handle_local_function(jsonata_keys, Args) ->
    case Args of
        [Lst] when is_list(Lst) ->
            UniqKeys = sets:from_list(
                lists:flatten([maps:keys(Map) || Map <- Lst])
            ),
            [any_to_binary(K) || K <- sets:to_list(UniqKeys)];
        [Map] when is_map(Map) ->
            [any_to_binary(K) || K <- maps:keys(Map)]
    end;
handle_local_function(split, Args) ->
    case Args of
        [Str] ->
            [<<>> | T] = lists:reverse(re:split(Str, "")),
            lists:reverse(T);
        [Str, ""] ->
            [<<>> | T] = lists:reverse(re:split(Str, "")),
            lists:reverse(T);
        [Str, Pat] ->
            string:split(Str, Pat, all);
        [Str, Pat, Lmt] ->
            All = string:split(Str, Pat, all),
            lists:sublist(All, Lmt)
    end;
handle_local_function(any_to_list, [Arg]) ->
    %% some truths are truer than others.
    case true of
        true when is_float(Arg) ->
            float_to_list(Arg, [short]);
        true when is_integer(Arg) ->
            integer_to_list(Arg);
        true when is_binary(Arg) ->
            binary_to_list(Arg);
        true when is_atom(Arg) ->
            atom_to_list(Arg);
        true ->
            Arg
    end;
handle_local_function(to_string, [Arg]) ->
    case true of
        %%
        %% misnomer - converts the value to a binary that is displayed as
        %% a string in the flow editor.
        true when is_binary(Arg) ->
            Arg;
        true when is_list(Arg) ->
            list_to_binary(Arg);
        true when is_atom(Arg) ->
            atom_to_binary(Arg);
        true when is_integer(Arg) ->
            integer_to_binary(Arg);
        true when is_float(Arg) ->
            list_to_binary(float_to_list(Arg, [short]));
        true ->
            list_to_binary(io_lib:format("~p", [Arg]))
    end;
handle_local_function(to_string, Arg) when is_list(Arg) ->
    list_to_binary(Arg);
handle_local_function(ered_millis, [Arg]) ->
    %% Arg contains the milliseconds for this evaluation, just
    %% return it - done.
    Arg;
handle_local_function(FunctionName, Args) ->
    erlang:error(jsonata_unsupported, [{FunctionName, Args}]).

evaluate_erlang(Expression) ->
    case erl_scan:string(Expression) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Parsed} ->
                    case
                        erl_eval:exprs(
                            Parsed,
                            [],
                            {value, fun handle_local_function/2}
                        )
                    of
                        {value, Result, _} ->
                            {ok, Result}
                    end;
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

%%
%%
jsonata_to_erlang(JSONataString) ->
    case erlang_red_jsonata_leex:string(JSONataString) of
        {ok, Tokens, _} ->
            case erlang_red_jsonata_parser:parse(Tokens) of
                {ok, Result} ->
                    {ok, Result};
                {error, Error} ->
                    {error, Error}
            end;
        R ->
            R
    end.

%%
%%
any_to_binary(V) when is_binary(V) ->
    V;
any_to_binary(V) when is_atom(V) ->
    atom_to_binary(V);
any_to_binary(V) when is_list(V) ->
    list_to_binary(V);
any_to_binary(V) ->
    V.
