require 'rbsigar'

def format_time(who)
  return Time.at(who.time).strftime("%b %e %H:%M")
end

sigar = Sigar.new

whos = sigar.who_list

whos.each do |who|
  puts who.user + "\t" + who.device + "\t" + format_time(who) + who.host
end
