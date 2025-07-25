-module(erlang_red_jsonata_test).

-include_lib("eunit/include/eunit.hrl").

count_test() ->
    ?assertEqual(
        {ok, 4},
        erlang_red_jsonata:execute(
            "$count($$.payload)",
            #{<<"payload">> => [1, 2, 3, 4]}
        )
    ),
    ?assertEqual(
        {ok, 4},
        erlang_red_jsonata:execute(
            "$count(msg.payload)",
            #{<<"payload">> => [1, 2, 3, 4]}
        )
    ).

parse_error_test() ->
    ?assertEqual(
        {error,
            {error,
                {1, erlang_red_jsonata_parser, ["syntax error before: ", []]}}},
        erlang_red_jsonata:execute(
            "$count($$.payload) /* comment",
            #{<<"payload">> => [1, 2, 3, 4]}
        )
    ).

replace_test() ->
    ?assertEqual(
        {ok, "ggkkggkkgg"},
        erlang_red_jsonata:execute(
            "$replace($$.payload,\"rr\",\"gg\")",
            #{<<"payload">> => "rrkkrrkkrr"}
        )
    ),

    %% using 'msg' for the '$$' variable. They are equivalent but '$$' is
    %% preferred.
    ?assertEqual(
        {ok, "ggkkggkkgg"},
        erlang_red_jsonata:execute(
            "$replace(msg.payload,\"rr\",\"gg\")",
            #{<<"payload">> => "rrkkrrkkrr"}
        )
    ).

replace_with_regexp_test() ->
    ?assertEqual(
        {ok, <<"Screen_Shot_2023-09-16_at_21.14.14.png">>},
        erlang_red_jsonata:execute(
            "$replace($$.payload,/[ \t\r]/,\"_\")",
            #{<<"payload">> => "Screen Shot 2023-09-16 at 21.14.14.png"}
        )
    ),
    ?assertEqual(
        {ok, <<"sadasd_dasd_____asdd__">>},
        erlang_red_jsonata:execute(
            "$replace($$.payload,/[ \t\r]/,\"_\")",
            #{<<"payload">> => "sadasd dasd \t \r asdd \r"}
        )
    ).

%% erlfmt:ignore
multistatement_test() ->
    ?assertEqual(
       {ok, '_underscore'},
       erlang_red_jsonata:execute(
         "$$.payload ; \"string\" ; 123 ; 321.123 ; $$._underscore",
         #{
           <<"payload">> => "rrkkrrkkrr",
           <<"_underscore">> => '_underscore'
          }
        )
    ).

single_expr_test() ->
    ?assertEqual(
        {ok, 4},
        erlang_red_jsonata:execute(
            "$$.payload",
            #{<<"payload">> => 4}
        )
    ),
    ?assertEqual(
        {ok, "string"},
        erlang_red_jsonata:execute(
            "\"string\"",
            #{<<"payload">> => 4}
        )
    ),
    ?assertEqual(
        {ok, 123},
        erlang_red_jsonata:execute(
            "123",
            #{<<"payload">> => 4}
        )
    ),
    ?assertEqual(
        {ok, 123.1},
        erlang_red_jsonata:execute(
            "123.1",
            #{<<"payload">> => 4}
        )
    ).

string_concat_test() ->
    Msg = #{
        <<"payload">> => "world"
    },
    ?assertEqual(
        {ok, "hello world"},
        erlang_red_jsonata:execute(
            "\"hello\" & \" \" & $$.payload",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
map_test() ->
    ?assertEqual(
        {ok, #{<<"key">> => "hello world"}},
        erlang_red_jsonata:execute(
            "{ \"key\": \"hello world\" }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{<<"key">> => value}},
        erlang_red_jsonata:execute(
            "{ key: value }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{<<"key">> => "hello world"}},
        erlang_red_jsonata:execute(
            "{ 'key': 'hello world' }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{<<"key">> => 4}},
        erlang_red_jsonata:execute(
            "{ \"key\": $$.payload }",
            #{<<"payload">> => 4}
        )
    ),

    ?assertEqual(
        {ok, #{<<"integer">> => 4, <<"float">> => 12.32}},
        erlang_red_jsonata:execute(
            "{ 'integer': 4, 'float': 12.32 }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{<<"key">> => 4, <<"key2">> => "value two"}},
        erlang_red_jsonata:execute(
            "{ \"key\": $$.payload, 'key2': 'value two' }",
            #{<<"payload">> => 4}
        )
    ),

    ?assertEqual(
        {ok, #{<<"Location">> => 4, <<"CapitalKey">> => "value two" }},
        erlang_red_jsonata:execute(
            "{ \"Location\": $$.payload, 'CapitalKey': $$.key2 }",
            #{ <<"payload">> => 4, <<"key2">> => "value two" }
        )
    ),

    Msg = #{
        <<"payload">> => #{
            <<"key">> => 4,
            <<"key2">> => #{
                <<"key3">> => 3,
                <<"key4">> => #{
                    <<"key5">> => 5
               }
            }
        }
    },

    ?assertEqual(
        {ok, #{<<"key">> => 4, <<"key2">> => 3, <<"key3">> => 5}},
        erlang_red_jsonata:execute(
            "{ \"key\": $$.payload.key,
               \"key2\": $$.payload.key2.key3,
               \"key3\": $$.payload.key2.key4.key5
          }",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
map_with_string_concat_test() ->
    Msg = #{
        <<"payload">> => #{
            <<"key">> => "4",
            <<"key2">> => #{
                <<"key3">> => "3",
                <<"key4">> => #{
                    <<"key5">> => "5"
               }
            }
        }
    },

    ?assertEqual(
        {ok, #{ <<"key">> => "4Hello 3 space 5" }},
        erlang_red_jsonata:execute(
            "/* commenter there */ { \"key\": $$.payload.key & \"Hello \" &
                   $$.payload.key2.key3 & \" space \" &
                 $$.payload.key2.key4.key5 } /* comment here */",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
map_with_string_binary_concat_test() ->
    Msg = #{
        <<"payload">> => #{
            <<"key">> => <<"4">>,
            <<"key2">> => #{
                <<"key3">> => <<"3">>,
                <<"key4">> => #{
                    <<"key5">> => <<"5">>
               }
            }
        }
    },

    ?assertEqual(
        {ok, #{ <<"key">> => "4Hello 3 space 5" }},
        erlang_red_jsonata:execute(
            "/* commenter there */ { \"key\": $$.payload.key & \"Hello \" &
                   $$.payload.key2.key3 & \" space \" &
                 $$.payload.key2.key4.key5 } /* comment here */",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
map_string_concat_with_int_test() ->
    ?assertEqual(
        {ok, "hello  world  1234 10 goodbye  cruel world"},
        erlang_red_jsonata:execute(
         "\"hello \" & \" world \" & \" 1234 \" & 10
                & \" goodbye \" & \" cruel \" & world",
            #{}
        )
    ),

    ?assertEqual(
        {ok, "yet another testhello  world  1234 10 goodbye  cruel world"},
        erlang_red_jsonata:execute(
         "$$.payload & \"hello \" & \" world \" & \" 1234 \" & 10
                & \" goodbye \" & \" cruel \" & world",
            #{<<"payload">> => <<"yet another test">>}
        )
    ).

%%
%% $toString is a ErlangRED special function from converting anything to
%% a binary - a string for all intense purposes in the flow editor. For
%% Erlang a binary for the flow editor a string, hence the name toString
%% which is what the user (of the flow editor) will be using.
tostring_from_anything_test() ->
    ?assertEqual(
        {ok, <<"what">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload)",
            #{<<"payload">> => <<"what">>}
        )
    ),
    ?assertEqual(
        {ok, <<"1753453562674_asd_asd____">>},
        erlang_red_jsonata:execute(
            "$toString(1753453562674 & \"_\" & $replace($$.file.name, /[\r\t\n ]/, \"_\"))",
            #{<<"file">> => #{<<"name">> => "asd asd \t \r"}}
        )
    ),
    ?assertEqual(
        {ok, <<"1753453562674_asd_asd____">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload & \"_\" & $replace($$.file.name, /[\r\t\n ]/, \"_\"))",
            #{
                <<"file">> => #{<<"name">> => "asd asd \t \r"},
                <<"payload">> => 1753453562674
            }
        )
    ),
    ?assertEqual(
        {ok, <<"when">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload)",
            #{<<"payload">> => "when"}
        )
    ),
    ?assertEqual(
        {ok, <<"12131312">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload)",
            #{<<"payload">> => 12131312}
        )
    ),
    ?assertEqual(
        {ok, <<"dddeee">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload & \"eee\")",
            #{<<"payload">> => "ddd"}
        )
    ),
    ?assertEqual(
        {ok, <<"dddeeefff">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload & \"eee\" & \"fff\")",
            #{<<"payload">> => "ddd"}
        )
    ),
    ?assertEqual(
        {ok, <<"12131312.123">>},
        erlang_red_jsonata:execute(
            "$toString($$.payload)",
            #{<<"payload">> => 12131312.123}
        )
    ).

%%
%% algorithmic is purposefully misspelled here. Why? Dunno.
%% erlfmt:ignore
algorithimc_test() ->
    ?assertEqual(
        {ok, 2},
        erlang_red_jsonata:execute(
            "1 + 1",
            #{}
        )
    ),
    ?assertEqual(
        {ok, 40},
        erlang_red_jsonata:execute(
            "$count($$.payload) * 5",
            #{<<"payload">> => [one, two, three, four, five, six, 'and', seven]}
        )
    ),
    ?assertEqual(
        {ok, 440},
        erlang_red_jsonata:execute(
            "$count($$.payload) * 5 * $length($$.str)",
            #{
                <<"payload">> => [one, two, three, four,
                                  five, six, 'and', seven],
                <<"str">> => "onetwothree"
            }
        )
    ),
    ?assertEqual(
        {ok, 38.743},
        erlang_red_jsonata:execute(
            "$$.payload.fuba.dad + 1.2 + 2.3 + $$.payload.name.name
                                                            + 3.243 + 4 * 6",
            #{<<"payload">> => #{<<"fuba">> => #{<<"dad">> => 4},
                                 <<"name">> => #{<<"name">> => 4}}}
        )
    ).

single_quote_string_test() ->
    ?assertEqual(
        {ok, "hello world"},
        erlang_red_jsonata:execute(
            "'hello' & ' ' & 'world'",
            #{}
        )
    ).

%% erlfmt:ignore
name_as_funct_argument_test() ->
    ?assertEqual(
        {ok, #{
            <<"float">> => 1.23,
            <<"key">> => <<"single quote strings">>,
            <<"key2">> => <<"value">>,
            <<"banaint">> => 4
        }},
        erlang_red_jsonata:execute(
            "{ \"key\": $toString('single quote strings'), banaint: 4,
                                   float: 1.23, key2: $toString(value) }",
            #{}
        )
    ).

empty_funct_arguments_test() ->
    ?assertEqual(
        {ok, <<"ok">>},
        erlang_red_jsonata:execute(
            "$millis() - $millis(); $string($pauseMillis(1))",
            #{}
        )
    ).

arithmetic_expressions_with_functions_test() ->
    ?assertEqual(
        {ok, 0},
        erlang_red_jsonata:execute(
            "$millis() - $millis()",
            #{}
        )
    ).

empty_funct_arguments_in_expr_test() ->
    ?assertEqual(
        {ok, #{<<"key">> => 0}},
        erlang_red_jsonata:execute(
            "{ key: $millis() - $millis() }",
            #{}
        )
    ).

unsupport_function_test() ->
    ?assertEqual(
        {exception, <<"jsonata unsupported function: {fromMillis,[]}">>},
        erlang_red_jsonata:execute(
            "1 + $fromMillis()",
            #{}
        )
    ).
now_datestampe_milli_test() ->
    ?assertEqual(
        {ok, <<"2025-07-25T13:35:54.0+00:00">>},
        erlang_red_jsonata:execute(
            "$now(1753450554005)",
            #{}
        )
    ).
now_datestampe_micro_test() ->
    ?assertEqual(
        {ok, <<"2025-07-25T13:56:26.0+00:00">>},
        erlang_red_jsonata:execute(
            "$now(1753451786639490)",
            #{}
        )
    ).
now_datestampe_no_argumenmt_test() ->
    ?assertEqual(
        ok,
        element(
            1,
            erlang_red_jsonata:execute(
                "$now()",
                #{}
            )
        )
    ).
now_datestampe_with_millis_test() ->
    ?assertEqual(
        ok,
        element(
            1,
            erlang_red_jsonata:execute(
                "$now($millis())",
                #{}
            )
        )
    ).

now_datestampe_with_millis_and_ampersand_test() ->
    ?assertEqual(
        {ok, "the time is now 2025-07-25T13:35:54.0+00:00 exactly"},
        erlang_red_jsonata:execute(
            "\"the time is now \" & $now(1753450554005) & \" exactly\"",
            #{}
        )
    ).

now_datestampe_with_timezone_test() ->
    ?assertEqual(
        {exception,
            <<"jsonata unsupported function: {jsonata_now,[1753451786639490,\"UTC\"]}">>},
        erlang_red_jsonata:execute(
            "$now(1753451786639490, \"UTC\")",
            #{}
        )
    ).

function_map_with_funct_test() ->
    ?assertEqual(
        {ok, [1, 2, 3]},
        erlang_red_jsonata:execute(
            "$map($$.payload, function ($v) { $v.col } )",
            #{
                <<"payload">> => [
                    #{<<"col">> => 1},
                    #{<<"col">> => 2},
                    #{<<"col">> => 3}
                ]
            }
        )
    ),
    ?assertEqual(
        {ok, ["a", "b", "c"]},
        erlang_red_jsonata:execute(
            "$map($$.payload, function ($v) { $v.col } )",
            #{
                <<"payload">> => [
                    #{<<"col">> => "a"},
                    #{<<"col">> => "b"},
                    #{<<"col">> => "c"}
                ]
            }
        )
    ),
    ?assertEqual(
        {ok, ["a", 2, "c"]},
        erlang_red_jsonata:execute(
            "$map($$.payload, function ($v) { $v.col.value } )",
            #{
                <<"payload">> => [
                    #{<<"col">> => #{<<"value">> => "a"}},
                    #{<<"col">> => #{<<"value">> => 2}},
                    #{<<"col">> => #{<<"value">> => "c"}}
                ]
            }
        )
    ).

function_sum_with_arrays_test() ->
    ?assertEqual(
        {ok, 21},
        erlang_red_jsonata:execute(
            "$sum($$.payload)",
            #{<<"payload">> => [1, 2, 3, 4, 5, 6]}
        )
    ),
    ?assertEqual(
        {ok, 22.1},
        erlang_red_jsonata:execute(
            "$sum($$.payload)",
            #{<<"payload">> => [1, 2, 3.4, 4.2, 5.1, 6.4]}
        )
    ).

function_sum_with_arrays_of_objects_test() ->
    ?assertEqual(
        {ok, 7},
        erlang_red_jsonata:execute(
            "$sum($map($$.payload, function ($v) { $v.col.value } ))",
            #{
                <<"payload">> => [
                    #{<<"col">> => #{<<"value">> => 2}},
                    #{<<"col">> => #{<<"value">> => 2}},
                    #{<<"col">> => #{<<"value">> => 3}}
                ]
            }
        )
    ).

array_with_index_test() ->
    ?assertEqual(
        {ok, 1},
        erlang_red_jsonata:execute(
            "$$.payload.key.key[0]",
            #{
                <<"payload">> => #{
                    <<"key">> => #{<<"key">> => [1, "hello world", 3]}
                }
            }
        )
    ),
    ?assertEqual(
        {ok, "hello world"},
        erlang_red_jsonata:execute(
            "$$.payload.key.key[1]",
            #{
                <<"payload">> => #{
                    <<"key">> => #{<<"key">> => [1, "hello world", 3]}
                }
            }
        )
    ),
    ?assertEqual(
        {ok, 3},
        erlang_red_jsonata:execute(
            "$$.payload.key.key[2]",
            #{
                <<"payload">> => #{
                    <<"key">> => #{<<"key">> => [1, "hello world", 3]}
                }
            }
        )
    ),
    ?assertEqual(
        {ok, 3},
        erlang_red_jsonata:execute(
            "$$.payload.key.key[-1]",
            #{
                <<"payload">> => #{
                    <<"key">> => #{<<"key">> => [1, "hello world", 3]}
                }
            }
        )
    ),
    ?assertEqual(
        {ok, "hello world"},
        erlang_red_jsonata:execute(
            "$$.payload.key.key[-2]",
            #{
                <<"payload">> => #{
                    <<"key">> => #{<<"key">> => [1, "hello world", 3]}
                }
            }
        )
    ),
    ?assertEqual(
        {ok, 1},
        erlang_red_jsonata:execute(
            "$$.payload.key.key[-3]",
            #{
                <<"payload">> => #{
                    <<"key">> => #{<<"key">> => [1, "hello world", 3]}
                }
            }
        )
    ).

split_string_test() ->
    ?assertEqual(
        {ok, ["one", "two", "three", "four"]},
        erlang_red_jsonata:execute(
            "$split($$.payload,\",\")",
            #{<<"payload">> => "one,two,three,four"}
        )
    ).

keys_of_single_maps_test() ->
    ?assertEqual(
        {ok, [<<"one">>, <<"three">>, <<"two">>]},
        erlang_red_jsonata:execute(
            "$sort($keys($$.payload))",
            #{
                <<"payload">> => #{
                    <<"one">> => "one",
                    <<"two">> => "two",
                    <<"three">> => "three"
                }
            }
        )
    ).

random_value_test() ->
    ?assertEqual(
        ok,
        element(
            1,
            erlang_red_jsonata:execute(
                "$random()",
                #{}
            )
        )
    ),
    ?assertEqual(
        ok,
        element(
            1,
            erlang_red_jsonata:execute(
                "\"d: \" & $random() & \" dd \"",
                #{}
            )
        )
    ),

    {ok, Val} = erlang_red_jsonata:execute("$random()", #{}),
    ?assertEqual(true, Val > 0 andalso Val < 1).

keys_of_list_of_maps_test() ->
    ?assertEqual(
        {ok, [<<"five">>, <<"four">>, <<"one">>, <<"three">>, <<"two">>]},
        erlang_red_jsonata:execute(
            "$sort($keys($$.payload))",
            #{
                <<"payload">> => [
                    #{
                        <<"one">> => "one",
                        <<"two">> => "two",
                        <<"three">> => "three"
                    },
                    #{
                        <<"one">> => "one",
                        <<"two">> => "two",
                        <<"three">> => "three"
                    },
                    #{
                        <<"one">> => "one",
                        <<"two">> => "two",
                        <<"five">> => "five"
                    },
                    #{
                        <<"one">> => "one",
                        <<"two">> => "two",
                        <<"four">> => "four"
                    }
                ]
            }
        )
    ).
