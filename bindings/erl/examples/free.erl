#!/usr/bin/env escript

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, Mem} = sigar:mem(S),
    io:format("\tTotal\tUsed\tFree~n"),
    io:format("Mem:    "),
    lists:map(
      fun(K) ->
              io:format("~w\t", [sigar:get_value(K, Mem)/1024]) end,
      [total, used, free]),
    io:format("~n"),
    {ok, Swap} = sigar:swap(S),
    io:format("Swap:    "),
    lists:map(
      fun(K) ->
              io:format("~w\t", [sigar:get_value(K, Swap)/1024]) end,
      [total, used, free]),
    io:format("~n"),
    io:format("RAM:   ~wMB~n", [sigar:get_value(ram, Mem)]),
    sigar:stop(S).


