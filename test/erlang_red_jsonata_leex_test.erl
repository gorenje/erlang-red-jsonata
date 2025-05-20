-module(erlang_red_jsonata_leex_test).

-include_lib("eunit/include/eunit.hrl").

did_it_fail(T) ->
    element(1, element(1, T)) == error.

lexical_test() ->
    {ok, FileData} = file:read_file(
        io_lib:format(
            "~s/leex.examples.json",
            [code:priv_dir(erlang_red_jsonata)]
        )
    ),
    TestCases = lists:map(
        fun(A) -> binary_to_list(A) end, json:decode(FileData)
    ),
    Lst = [{erlang_red_jsonata_leex:string(T), T} || T <- TestCases],

    FinalResult = lists:filter(fun(A) -> did_it_fail(A) end, Lst),
    ?assertEqual([], FinalResult).
