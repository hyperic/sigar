#!/usr/bin/env escript

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    case sigar:loadavg(S) of
        {ok, Avg} ->
            lists:map(
              fun(A) ->
                      io:format("~w% ", [A]) end,
              Avg);
        {error, Err} ->
            Err
    end,
    io:format("~n"),
    sigar:stop(S).


