#!/usr/bin/env escript

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, Names} = sigar:net_interface_list(S),
    lists:map(
      fun(K) ->
              {ok, Ifconfig} = sigar:net_interface_config(S, K),
              io:format("~s ~s~n", [K, sigar:get_value(address, Ifconfig)]) end,
      Names),
    sigar:stop(S).


