#
# Copyright (c) 2007 Hyperic, Inc.
# Copyright (c) 2010 VMware, Inc.
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

require 'sigar'

sigar = Sigar.new
mem = sigar.mem
swap = sigar.swap

puts "\tTotal\tUsed\tFree"

puts "Mem:    " +
     (mem.total / 1024).to_s + "\t" +
     (mem.used / 1024).to_s + "\t" +
     (mem.free/ 1024).to_s

puts "Swap:   " +
     (swap.total / 1024).to_s + "\t" +
     (swap.used / 1024).to_s + "\t" +
     (swap.free/ 1024).to_s

puts "RAM:    " + mem.ram.to_s + "MB";
