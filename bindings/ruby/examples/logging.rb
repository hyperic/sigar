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

# Example illustrating the use of the optional logger.

require 'sigar'
require 'logger'

sigar = Sigar.new
sigar.log_level = Sigar::LOG_DEBUG

logger = Logger.new(STDERR)
logger.datetime_format = "%H:%M:%S"

# Call something that has DEBUG level logging.
fslist = sigar.file_system_list

# cache data
fslist.each do |fs|
  dir_name = fs.dir_name
  usage = sigar.file_system_usage(dir_name)
end

puts "Calling file_system_usage() for each filesystem"

puts "\nwith Logger:"

sigar.logger = logger

fslist.each do |fs|
  dir_name = fs.dir_name
  usage = sigar.file_system_usage(dir_name)
end

puts "\nwith Proc:"

sigar.logger = Proc.new do |level, msg| 
  puts "Level #{level}: #{msg}"
end

fslist.each do |fs|
  dir_name = fs.dir_name
  usage = sigar.file_system_usage(dir_name)
end
