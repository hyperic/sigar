#!/usr/bin/env escript

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, FsList} = sigar:file_system_list(S),
    lists:map(
      fun(Fs) ->
              io:format("~s ~n", [sigar:get_value(dir_name, Fs)]) end,
      FsList),
    sigar:stop(S).


