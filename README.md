Erlang Parser for JSONata
=====

Erlang parser implementation for the [JSONata](https://jsonata.org) transform language.

JSONata plays a central role in [Node-RED](https://nodered.org) - the low-code visual flow-based programming environment.

This is an implementation for the [Erlang-RED](https://github.com/gorenje/erlang-red) codebase. Erlang-RED is the low-code visual flow-based programming environment inspired by Node-RED!

What is [flow based programming](http://wiki.c2.com/?FlowBasedProgramming)? A programming paradigm focussed on message passing between independent computational units.

Architecture
----

This library transforms JSONata stanzas into valid Erlang code which are intended to be executed in the presence of a `Msg` map.

For example:

```jsonata
$$.payload.fuba.dad + 1 + 2 + $$.payload.name.name + 3 + 4 * 6
```

becomes:

```erlang
fun (Msg) ->
    maps:get(dad, maps:get(fuba, maps:get(payload, Msg))) + 1 + 2 +
         maps:get(name, maps:get(name, maps:get(payload, Msg))) + 3 + 4 * 6
end.
```

JSONata Support
----

At the time of writing, this library supports a minimum set of functionality of the original JSONata language. Of all the [functions](https://github.com/jsonata-js/jsonata/blob/0159fe9d7047b8dd5e09f5a19b3a114f298306e8/src/functions.js#L2059-L2067) plus the [Node-RED](https://github.com/node-red/node-red/blob/0f653ed7b2640feba8885e48b9448df7d42acaf0/packages/node_modules/%40node-red/util/lib/util.js#L705-L734) extensions, this supports:

| Function | Comment |
| ---- | ------- |
| $count | Supported |
| $keys | Supported |
| $length | Supported |
| $map | Supported |
| $millis | Supported - including the requirement that multiple calls produce the same value |
| $replace | Supported but no regular expressions |
| $split | Supported |
| $string | Supported |
| $sum | Supported |
| $pauseMillis | Erlang-RED special: pause this many milli seconds |
| $toString | Erlang-RED special basically the same as $string |

Functions are defined in two places [in the parser](https://github.com/gorenje/erlang-red-jsonata/blob/f835ebb55c6df7f180ceeaeffed21a51125a25b7/src/erlang_red_jsonata_parser.yrl#L423-L484) or in the [evaluator](https://github.com/gorenje/erlang-red-jsonata/blob/f835ebb55c6df7f180ceeaeffed21a51125a25b7/src/erlang_red_jsonata.erl#L50-L109).

There is no particular timeline to extend that list, functionality is being added as needed by the [flow tests](https://github.com/gorenje/erlang-red-flow-testsuite) the define the current behaviour of existing Node-RED nodes.

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit
