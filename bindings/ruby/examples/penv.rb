require 'rbsigar'

def output(sigar, pid)
  env = sigar.proc_env(pid)
  env.each do |key, val|
    puts key + "=" + val
  end
end

sigar = Sigar.new

ARGV.each do |pid|
  output(sigar, pid.to_i)
end
