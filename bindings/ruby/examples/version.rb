#
# Copyright (c) 2009 SpringSource, Inc.
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
require 'rbconfig'

puts "Sigar version......." + Sigar::VERSION
puts "Build date.........." + Sigar::BUILD_DATE
puts "SCM rev............." + Sigar::SCM_REVISION

sys = Sigar.new.sys_info
puts "OS description......" + sys.description
puts "OS name............." + sys.name
puts "OS arch............." + sys.arch
puts "OS machine.........." + sys.machine
puts "OS version.........." + sys.version
puts "OS patch level......" + sys.patch_level
puts "OS vendor..........." + sys.vendor
puts "OS vendor version..." + sys.vendor_version
if (sys.vendor_code_name != nil)
    puts "OS code name........" + sys.vendor_code_name
end

puts "Ruby version........" + RUBY_VERSION
puts "Ruby build vendor..." + ::Config::CONFIG['build_vendor']
puts "Ruby archdir........" + ::Config::CONFIG['archdir']
