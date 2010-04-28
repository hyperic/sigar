#!/usr/bin/env escript
%
% Copyright (c) 2009 SpringSource, Inc.
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%

-include(sigar).

main(_) ->
    {ok, S} = sigar:start(),
    {ok, FsList} = sigar:file_system_list(S),
    lists:map(
      fun(Fs) ->
              io:format("~s ~n", [sigar:get_value(dir_name, Fs)]) end,
      FsList),
    sigar:stop(S).


