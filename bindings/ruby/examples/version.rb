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
