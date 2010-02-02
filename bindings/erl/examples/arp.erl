#!/usr/bin/env escript
%%! -pz ebin
-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, List} = sigar:arp_list(S),
    lists:map(
      fun(Arp) ->
              lists:map(
                fun(K) ->
                        io:format("~s\t", [sigar:get_value(K, Arp)]) end,
                [address, hwaddr, type, ifname]),
              io:format("~n")
      end, List),
    sigar:stop(S).
