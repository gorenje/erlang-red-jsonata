%%% % @noformat

-module(erlang_red_jsonata_parser_test).

-include_lib("eunit/include/eunit.hrl").

%%
%% These tests really only test the use cases I have from JSONata. There
%% basically three usecase for me:
%%
%%   - function calls with msg values as parameters
%%   - creation of JSON objects using msg values
%%   - string concatenation using ampersand operator.
%%
%% therefore these tests cover those use cases.
%%
%% Other uses for JSONata aren't - at time of writing - covered and remain
%% as an exercise for the reader.
%%

foreach_parser_test_() ->
    Tests = [
      {
       map_one_key_and_value_test,
       "{ \"key\": $$.payload.key2 }",
       "fun (Msg) -> #{
          <<\"key\">> => maps:get(<<\"key2\">>, maps:get(<<\"payload\">>, Msg))
        } end."
      },
      {
       funct_simple_key_using_msg_test,
       "$count(msg.payload)",
       "fun (Msg) -> erlang:length(maps:get(<<\"payload\">>, Msg)) end."
      },
      {
       funct_simple_key_test,
       "$count($$.payload)",
       "fun (Msg) -> erlang:length(maps:get(<<\"payload\">>, Msg)) end."
      },
      {
       map_handle_underscore_as_key_value,
       "{ \"val\": $$._payload._key }",
       "fun (Msg) ->
           #{ <<\"val\">> => maps:get(<<\"_key\">>, maps:get(<<\"_payload\">>, Msg)) }
        end."
      },
      {
       funct_three_arguments,
       "$replace( $$.context._datafile, \".json\", \".csv\")",
       "fun (Msg) ->
            lists:flatten(string:replace(maps:get(<<\"_datafile\">>,
                          maps:get(<<\"context\">>, Msg)),
                           \".json\", \".csv\", all))
        end."
      },
      {
       replace_spaces_in_file_names,
       "$replace($$.file.name, /[abce ]/, \"_\")",
       "fun (Msg) ->
            re:replace(maps:get(<<\"name\">>, maps:get(<<\"file\">>, Msg)),
                     \"[abce ]\", \"_\",
                    [dotall,dollar_endonly,caseless,global,{return,list}])
        end."
      },
      {
       funct_replace_with_four_args,
       "$replace( $$.context._datafile, \".json\", \".csv\", 20)",
       "fun (Msg) ->
            lists:flatten(string:replace(maps:get(<<\"_datafile\">>,
                             maps:get(<<\"context\">>, Msg)),
                           \".json\", \".csv\"))
        end."
      },
      {
       funct_with_nested_keys_test,
       "$count($$.payload.key1.key2.key3.key4)",
       "fun (Msg) ->
            erlang:length(maps:get(<<\"key4\">>, maps:get(<<\"key3\">>,
                             maps:get(<<\"key2\">>, maps:get(<<\"key1\">>,
                                maps:get(<<\"payload\">>, Msg))))))
        end."
      },
      {
       %% 'msg' and '$$' are interchangeable but '$$' is preferred and should
       %% be used in JSONata expressions.
       parser_map_multiple_key_values_test,
       "{ \"key\": msg.payload.key2,
          \"key2\": $$.map.key1.val3,
          \"key3\": msg.map.key2 }",
       "fun (Msg) -> #{
            <<\"key\">> => maps:get(<<\"key2\">>, maps:get(<<\"payload\">>, Msg)),
            <<\"key2\">> => maps:get(<<\"val3\">>, maps:get(<<\"key1\">>, maps:get(<<\"map\">>, Msg))),
            <<\"key3\">> => maps:get(<<\"key2\">>, maps:get(<<\"map\">>, Msg))
          } end."
      },
      {
       %% 'msg' and '$$' are interchangeable but '$$' is preferred and should
       %% be used in JSONata expressions.
       parser_map_one_key_and_value_with_msg_test,
       "{ \"key\": msg.payload.key2 }",
       "fun (Msg) -> #{
           <<\"key\">> => maps:get(<<\"key2\">>, maps:get(<<\"payload\">>, Msg))
        } end."
      },
      {
       single_expr_of_various_forms,
       "$$.payload ; \"string\" ; 123 ; 321.123 ; $$._underscore",
       "fun (Msg) ->
            maps:get(<<\"payload\">>, Msg),
            \"string\",
            123, 321.123, maps:get(<<\"_underscore\">>, Msg)
        end."
      },
      {
       %% comments have to be statements
       ignore_comments_but_include_expressions,
       "/* comment asd asd */ ; $$.payload ; /* comment asd asd 12 sdsa */",
       "fun (Msg) ->
          maps:get(<<\"payload\">>, Msg)
        end."
      },
      {
       %% comments have to be statements
       ignore_comments_but_include_expressions_2,
       "/* comment asd asd */ ; $$.payload ; /* comment asd asd 12 sdsa */ ;
            $$.fubar",
       "fun (Msg) ->
          maps:get(<<\"payload\">>, Msg), maps:get(<<\"fubar\">>, Msg)
        end."
      },
      {
       %% comments have to be statements
       ignore_comments,
       "/* comment asd asd */",
       "fun (Msg) ->
          Msg
        end."
      },
      {
       string_concatenation,
       "\"one\" & \"two\" & $$.payload",
       "fun (Msg) ->
          \"one\" ++ \"two\" ++ any_to_list(maps:get(<<\"payload\">>, Msg))
        end."
      },
      {
       string_concatenation_as_key_value_in_map,
       "/* comment to be ignored */ { \"key\" : \"fubar \" & $$.payload }
          /* ignore this comment too */",
       "fun (Msg) ->
          #{ <<\"key\">> => \"fubar \" ++ any_to_list(maps:get(<<\"payload\">>, Msg)) }
        end."
      },
      {
        string_concat_with_int_and_name,
        "\"hello \" & \" world \" & \" 1234 \" & 10
                & \" goodbye \" & \" cruel \" & world",
        "fun (Msg) ->
            \"hello \" ++ \" world \" ++ \" 1234 \" ++ \"10\" ++
                     \" goodbye \" ++ \" cruel \" ++ any_to_list(world)
        end."
      },
      {
        to_string_functionality,
        "$toString($$.payload)",
        "fun (Msg) ->
           to_string(maps:get(<<\"payload\">>, Msg))
        end."
      },
      {
        algorithmic_expressions_basic,
        "1 + 2",
        "fun (Msg) ->
           1 + 2
        end."
      },

      {
        algorithmic_expressions_integer_only,
        "$$.payload.fuba.dad + 1 + 2 + $$.payload.name.name + 3 + 4 * 6",
        "fun (Msg) ->
             maps:get(<<\"dad\">>, maps:get(<<\"fuba\">>,
                            maps:get(<<\"payload\">>, Msg))) + 1 + 2 +
                          maps:get(<<\"name\">>, maps:get(<<\"name\">>,
                                    maps:get(<<\"payload\">>, Msg))) + 3 + 4 * 6
        end."
      },
      {
        algorithmic_expressions_integer_and_float,
        "$$.payload.fuba.dad + 1.2 + 2.3 + $$.payload.name.name + 3.243 + 4 * 6",
        "fun (Msg) ->
               maps:get(<<\"dad\">>, maps:get(<<\"fuba\">>,
                           maps:get(<<\"payload\">>, Msg))) + 1.2 + 2.3 +
                                  maps:get(<<\"name\">>, maps:get(<<\"name\">>,
                    maps:get(<<\"payload\">>, Msg))) + 3.243 + 4 * 6
        end."
      },
      {
        map_support_non_string_key_names,
        "{ \"key\": 'single quote strings', banaint: 4, float: 1.23,
                                                             key2: value }",
        "fun (Msg) ->
            #{ <<\"key\">> => \"single quote strings\", <<\"banaint\">> => 4,
                            <<\"float\">> => 1.23, <<\"key2\">> => value }
        end."
      },
      {
        functions_with_no_arguments,
        "$millis()",
        "fun (Msg) ->
          EREDMillis = erlang:system_time(millisecond), ered_millis(EREDMillis)
        end."
      },
      {
        pause_millis_function,
        "$pauseMillis(1000)",
        "fun (Msg) ->
          timer:sleep(1000)
        end."
      },
      {
        arithmetic_expressions_as_key_value,
        "{ key: $millis() - $millis() }",
        "fun (Msg) ->
          EREDMillis = erlang:system_time(millisecond),
            #{ <<\"key\">> => ered_millis(EREDMillis) - ered_millis(EREDMillis) }
        end."
      },
      {
        empty_array,
        "[]",
        "fun (Msg) ->
           []
        end."
      },
      {
        array_with_funct_calls,
        "[ $toString($$.ary), $toString($$.ary2), \"four\"]",
        "fun (Msg) ->
           [to_string(maps:get(<<\"ary\">>, Msg)),
                                to_string(maps:get(<<\"ary2\">>, Msg)), \"four\"]
        end."
      },
      {
        array_index_access,
        "$$.payload.key.key[2]",
        "fun (Msg) ->
            lists:nth(3, maps:get(<<\"key\">>, maps:get(<<\"key\">>,
                                         maps:get(<<\"payload\">>, Msg))))
        end."
      },
      {
        array_index_access_zero_based_converted,
        "$$.payload.key.key[0]",
        "fun (Msg) ->
            lists:nth(1, maps:get(<<\"key\">>, maps:get(<<\"key\">>,
                                             maps:get(<<\"payload\">>, Msg))))
        end."
      },
      {
        array_index_access_negative_based_from_the_back,
        "$$.payload.key.key[-1]",
        "fun (Msg) ->
            lists:nth(1, lists:reverse(maps:get(<<\"key\">>,
                 maps:get(<<\"key\">>, maps:get(<<\"payload\">>, Msg)))))
        end."
      },
      {
        function_definition_as_argument,
        "$sum($map($$.payload, function($v) { $v.col2 }))",
        "fun (Msg) ->
            lists:sum(lists:map(fun(V) -> maps:get(<<\"col2\">>, V) end,
                                               maps:get(<<\"payload\">>, Msg)))
         end."
      },
      {
        function_sum_of_array,
        "$sum($$.payload)",
        "fun (Msg) ->
            lists:sum(maps:get(<<\"payload\">>, Msg))
         end."
      },
      {
        function_map_of_objects,
        "$map($$.payload, function($r) { $r.attr })",
        "fun (Msg) ->
            lists:map(fun(V) -> maps:get(<<\"attr\">>, V) end, maps:get(<<\"payload\">>, Msg))
         end."
      },
      {
        function_keys_of_objects,
        "$keys($$.payload)",
        "fun (Msg) ->
             jsonata_keys(maps:get(<<\"payload\">>, Msg))
         end."
      },
      {
        function_split_of_strings,
        "$count($split($$.payload))",
        "fun (Msg) ->
            erlang:length(split(maps:get(<<\"payload\">>, Msg)))
         end."
      },
      {
        capital_letter_key_names_to_atoms,
        "{ 'Location' : 'value', \"Location\": \"VLAUE\" }",
        "fun (Msg) ->
           #{ <<\"Location\">> => \"value\", <<\"Location\">> => \"VLAUE\" }
        end."
      },
      {
        array_with_content,
        "[1, 2, \"asdasd\", $$.payload]",
        "fun (Msg) ->
           [1, 2, \"asdasd\", maps:get(<<\"payload\">>, Msg)]
        end."
      }
     ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_jsonata_leex:string(SrcString),

                         {ok, Result} = erlang_red_jsonata_parser:parse(Tokens),

                         ResultStringRmSpace = list_to_binary(
                                                 re:replace(ResultString,
                                                            "\\s+", " ",
                                                            [global])
                                                ),

                         ?assertEqual(ResultStringRmSpace, Result)
                 end
                } || {TestCaseName, SrcString, ResultString} <- Tests],
    {inparallel, TestList}.
