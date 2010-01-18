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
