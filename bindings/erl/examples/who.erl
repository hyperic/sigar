#!/usr/bin/env escript

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, List} = sigar:who_list(S),
    lists:map(
      fun(Who) ->
              lists:map(
                fun(K) ->
                        io:format("~s\t", [sigar:get_value(K, Who)]) end,
                [user, device, host]),
              io:format("~n")
      end, List),
    sigar:stop(S).
