-module(erlang_red_jsonata).

-export([
    evaluate_erlang/1,
    execute/2,
    jsonata_to_erlang/1,
    compile_to_function/1
]).

%%
%% The UberAPI to the jsonata_leex and jsonata_parser modules.
%%
%% This module provides a single interface for evaluating a JSONata stanza
%% in the presence of the Msg object.
%%
-spec execute(
    JSONata :: string(),
    Msg :: map()
) ->
    {ok, ReturnValue :: any()}
    | {error, ErrMsg :: string()}
    | {unsupported, Description :: string()}
    | {exception, {Error :: atom(), Message :: tuple(), Stack :: [tuple()]}}.
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
            {unsupported,
                list_to_binary(
                    io_lib:format(
                        "jsonata unsupported function: ~p", element(3, H)
                    )
                )};
        E:M:S ->
            {exception, {E, M, S}}
    end.

%%
%% Pre-compile the JSONata to a BEAM function to be used later, multiple times
%% as messages come in.
-spec compile_to_function(
    JSONata :: string()
) ->
    {ok, Function :: fun()}
    | {error, ErrMsg :: string()}
    | {exception, {Error :: atom(), Message :: tuple(), Stack :: [tuple()]}}.
compile_to_function(JSONata) when is_binary(JSONata) ->
    compile_to_function(binary_to_list(JSONata));
compile_to_function(JSONata) ->
    try
        case jsonata_to_erlang(JSONata) of
            {ok, ErlangCode} ->
                case evaluate_erlang(binary_to_list(ErlangCode)) of
                    {ok, Func} ->
                        {ok, Func};
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
        E:M:S ->
            {exception, {E, M, S}}
    end.


%%
%% Inspired by this blog post:
%% https://grantwinney.com/how-to-evaluate-a-string-of-code-in-erlang-at-runtime/
%%
handle_local_function(jsonata_priv_dir, [Args]) when is_binary(Args) ->
    code:priv_dir(binary_to_atom(Args));
handle_local_function(jsonata_priv_dir, [Args]) when is_atom(Args) ->
    code:priv_dir(Args);
handle_local_function(jsonata_priv_dir, [Args]) when is_list(Args) ->
    code:priv_dir(list_to_atom(Args));
%%
handle_local_function(jsonata_length, [Args]) when is_binary(Args) ->
    erlang:byte_size(Args);
handle_local_function(jsonata_length, [Args]) ->
    erlang:length(Args);
%% --> $keys
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
%% --> $split
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
%%
handle_local_function(any_to_list, [Arg]) when is_float(Arg) ->
    float_to_list(Arg, [short]);
handle_local_function(any_to_list, [Arg]) when is_integer(Arg) ->
    integer_to_list(Arg);
handle_local_function(any_to_list, [Arg]) when is_binary(Arg) ->
    binary_to_list(Arg);
handle_local_function(any_to_list, [Arg]) when is_atom(Arg) ->
    atom_to_list(Arg);
handle_local_function(any_to_list, [Arg]) ->
    Arg;
%% --> $toString
handle_local_function(to_string, [Arg]) when is_binary(Arg) -> Arg;
handle_local_function(to_string, [Arg]) when is_list(Arg) ->
    list_to_binary(Arg);
handle_local_function(to_string, [Arg]) when is_atom(Arg) ->
    atom_to_binary(Arg);
handle_local_function(to_string, [Arg]) when is_integer(Arg) ->
    integer_to_binary(Arg);
handle_local_function(to_string, [Arg]) when is_float(Arg) ->
    list_to_binary(float_to_list(Arg, [short]));
handle_local_function(to_string, [Arg]) ->
    list_to_binary(io_lib:format("~p", [Arg]));
handle_local_function(to_string, Arg) when is_list(Arg) ->
    list_to_binary([any_to_list(A) || A <- Arg]);
%% --> $now()
handle_local_function(jsonata_now, []) ->
    iso_8601_datestamp(erlang:timestamp());
handle_local_function(
    jsonata_now, [Arg]
) when is_integer(Arg), Arg > 1000_000_000_000_000 ->
    % assume we're dealing with microseconds
    MegaSec = Arg div 1000_000_000_000,
    Ts = {
        MegaSec,
        Arg div 1000_000 - MegaSec * 1000_000,
        Arg rem 1000_000
    },
    iso_8601_datestamp(Ts);
handle_local_function(
    jsonata_now, [Arg]
) when is_integer(Arg) ->
    % these are assumed to be milliseconds
    MegaSec = Arg div 1000_000_000,
    Ts = {
        MegaSec,
        Arg div 1000 - MegaSec * 1000_000,
        Arg rem 1000_000
    },
    iso_8601_datestamp(Ts);
%%
handle_local_function(ered_millis, [Arg]) ->
    %% Arg contains the milliseconds for this evaluation, just
    %% return it - done.
    Arg;
%% --> $formatBase
handle_local_function(
    jsonata_formatbase,
    [Val, Base]
) when is_integer(Val), is_integer(Base) ->
    string:lowercase(integer_to_binary(Val, Base));
handle_local_function(
    jsonata_formatbase,
    [Val, Base]
) when is_float(Val), is_integer(Base) ->
    V = element(1, string:to_integer(lists:nth(1, io_lib:format("~p", [Val])))),
    handle_local_function(jsonata_formatbase, [V, Base]);
%% ---> $pad(...)
handle_local_function(
    jsonata_pad,
    [Str, Length]
) when
    is_binary(Str), is_integer(Length), Length < 0;
    is_list(Str), is_integer(Length), Length < 0
->
    list_to_binary(string:pad(Str, Length, leading, " "));
handle_local_function(
    jsonata_pad,
    [Str, Length]
) when
    is_binary(Str), is_integer(Length), Length > 0;
    is_list(Str), is_integer(Length), Length > 0
->
    list_to_binary(string:pad(Str, Length, trailing, " "));
handle_local_function(
    jsonata_pad,
    [Str, Length, Char]
) when
    is_binary(Str), is_integer(Length), is_list(Char), Length < 0;
    is_list(Str), is_integer(Length), is_binary(Char), Length < 0;
    is_binary(Str), is_integer(Length), is_binary(Char), Length < 0;
    is_list(Str), is_integer(Length), is_list(Char), Length < 0
->
    list_to_binary(string:pad(Str, Length * -1, leading, Char));
handle_local_function(
    jsonata_pad,
    [Str, Length, Char]
) when
    is_binary(Str), is_integer(Length), is_binary(Char), Length > 0;
    is_list(Str), is_integer(Length), is_list(Char), Length > 0;
    is_binary(Str), is_integer(Length), is_list(Char), Length > 0;
    is_list(Str), is_integer(Length), is_binary(Char), Length > 0
->
    list_to_binary(string:pad(Str, Length, trailing, Char));
%% ---> $substring(...)
handle_local_function(
    jsonata_substring,
    [Str, Start]
) when is_binary(Str), is_integer(Start) ->
    handle_local_function(jsonata_substring, [binary_to_list(Str), Start]);
handle_local_function(
    jsonata_substring,
    [Str, Start]
) when is_list(Str), is_integer(Start) ->
    handle_local_function(jsonata_substring, [Str, Start, length(Str)]);
handle_local_function(
    jsonata_substring,
    [Str, Start, Length]
) when
    is_list(Str),
    is_integer(Start),
    is_integer(Length),
    Length > 0,
    Start >= 0
->
    list_to_binary(string:slice(Str, Start, Length));
handle_local_function(
    jsonata_substring,
    [Str, Start, Length]
) when
    is_list(Str),
    is_integer(Start),
    is_integer(Length),
    Length > 0,
    Start < 0
->
    list_to_binary(string:slice(Str, length(Str) + Start, Length));
handle_local_function(
    jsonata_substring,
    [Str, Start, Len]
) when is_binary(Str), is_integer(Start), is_integer(Len) ->
    handle_local_function(jsonata_substring, [binary_to_list(Str), Start, Len]);
%% -------> fall through to unsupported
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
any_to_binary(V) when is_atom(V) ->
    atom_to_binary(V);
any_to_binary(V) when is_list(V) ->
    list_to_binary(V);
any_to_binary(V) ->
    V.

any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V);
any_to_list(V) when is_float(V) ->
    float_to_list(V, [short]);
any_to_list(V) ->
    V.

%%
%%
iso_8601_datestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_datetime(Timestamp),
    list_to_binary(
        io_lib:format(
            "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00",
            [Year, Month, Day, Hour, Min, Sec]
        )
    ).
