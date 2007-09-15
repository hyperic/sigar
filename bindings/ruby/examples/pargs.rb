require 'rbsigar'

def output(sigar, pid)
  args = sigar.proc_args(pid)
  exe = sigar.proc_exe(pid);
  puts "exe=" + exe.name
  puts "cwd=" + exe.cwd

  args.each do |arg|
    puts "   " + "=>" + arg + "<="
  end
end

sigar = Sigar.new

ARGV.each do |pid|
  output(sigar, pid.to_i)
end
