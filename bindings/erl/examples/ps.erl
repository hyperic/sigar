#!/usr/bin/env escript

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, Mem} = sigar:proc_mem(S, "$$"),
    io:format("Size\tResident~n"),
    lists:map(
      fun(K) ->
              io:format("~w\t", [sigar:get_value(K, Mem)/1024]) end,
      [size, resident]),
    io:format("~n"),
    sigar:stop(S).


