#
# Copyright (c) 2007 Hyperic, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import os, sigar;

sg = sigar.open()
mem = sg.mem()
swap = sg.swap()
sg.close()

print "\tTotal\tUsed\tFree"

print "Mem:\t",\
    (mem.total() / 1024), \
    (mem.used() / 1024), \
    (mem.free() / 1024)

print "Swap:\t", \
    (swap.total() / 1024), \
    (swap.used() / 1024), \
    (swap.free() / 1024)

print "RAM:\t", mem.ram(), "MB"

