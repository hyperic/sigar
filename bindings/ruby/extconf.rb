require 'mkmf'

extension_name = 'rbsigar'

print 'Ruby platform=' + RUBY_PLATFORM + "\n"

case RUBY_PLATFORM
when /darwin/
  os = 'darwin'
  sdks = Dir.glob('/Developer/SDKs/MacOSX10.*.sdk').sort.reverse
  if sdks.length == 0
    print "Xcode Developer Tools not installed\n"
    print "Download from http://developer.apple.com/technology/xcode.html\n"
    exit 1
  else
    print "Available SDKs...\n(*) " + sdks.join("\n    ") + "\n"
    sdk = sdks[0]
  end
  if File.file?("/usr/include/libproc.h")
    $CPPFLAGS += ' -DDARWIN_HAS_LIBPROC_H'
  end
  $CPPFLAGS += ' -DDARWIN -I/Developer/Headers/FlatCarbon -isysroot ' + sdk
  $LDFLAGS += ' -Wl,-syslibroot,' + sdk + ' -framework CoreServices -framework IOKit'
when /bsd/
  os = 'darwin'
  have_library("kvm")
when /mswin32|mingw|cygwin|bccwin32/
  os = 'win32'
  $CPPFLAGS += ' -DWIN32'
  is_win32 = true
  have_library("kernel32")
  have_library("user32")
  have_library("advapi32")
  have_library("ws2_32")
  have_library("netapi32")
  have_library("shell32")
  have_library("pdh")
  have_library("version")
when /linux/
  os = 'linux'
when /solaris|sun/
  os = 'solaris'
  have_library("nsl")
  have_library("socket")
  have_library("kstat")
when /hpux/
  os = 'hpux'
  #XXX have_libary no workie on hpux?
  $LDFLAGS += ' -lnsl -lnm'
when /aix/
  os = 'aix'
  have_library("odm")
  have_library("cfg")
  have_library("perfstat")
else
  os = RUBY_PLATFORM
end

osdir = "../../src/os/#{os}"
$CPPFLAGS += ' -I../../include' + ' -I' + osdir
$CPPFLAGS += ' -U_FILE_OFFSET_BITS' unless is_win32

system('perl -Mlib=.. -MSigarWrapper -e generate Ruby .')

$distcleanfiles = ['rbsigar_generated.rx']
#XXX seems mkmf forces basename on srcs
#XXX should be linking against libsigar anyhow
Dir["../../src/*.c","#{osdir}/*.c"].each do |file|
  cf = File.basename(file)
  print file + ' -> ' + cf + "\n"
  if is_win32
    File.copy(file, cf)
  else
    File.symlink(file, cf) unless File.file?(cf)
  end
  $distcleanfiles.push(cf)
end

dir_config(extension_name)

create_makefile(extension_name)
