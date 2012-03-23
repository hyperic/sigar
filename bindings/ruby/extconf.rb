#
# Copyright (c) 2007, 2009 Hyperic, Inc.
# Copyright (c) 2009 SpringSource, Inc.
# Copyright (c) 2010-2012 VMware, Inc.
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

require 'mkmf'
require 'rbconfig'

extension_name = 'sigar'

print 'Ruby platform=' + RUBY_PLATFORM + "\n"

case RUBY_PLATFORM
when /darwin/
  os = 'darwin'
  if File.file?("/usr/include/libproc.h")
    $CPPFLAGS += ' -DDARWIN_HAS_LIBPROC_H'
  end
  $CPPFLAGS += ' -DDARWIN'
  $LDFLAGS += ' -framework CoreServices -framework IOKit'
when /bsd/
  os = 'darwin'
  have_library("kvm")
when /mswin|mingw|cygwin|bccwin/
  os = 'win32'
  require 'ftools'
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

if RUBY_VERSION > '1.8.4'
  $CPPFLAGS += ' -DRB_HAS_RE_ERROR'
end
if RUBY_VERSION >= '1.9.0'
  $CPPFLAGS += ' -DRB_RUBY_19'
end

#incase of nfs shared dir...
unless is_win32
  if File.exist?('Makefile')
    cmd = 'make distclean'
    print cmd + "\n"
    system(cmd)
  end
  Dir["./*.c"].each do |file|
    if File.lstat(file).symlink?
      print "unlink #{file}\n"
      File.delete(file)
    end
  end
end

$distcleanfiles = ['rbsigar_generated.rx','sigar_version.c']

system('perl -Mlib=.. -MSigarWrapper -e generate Ruby .')
libname = extension_name + '.' + CONFIG['DLEXT']
filters =
  'ARCHNAME=' + RUBY_PLATFORM + ' ' +
  'ARCHLIB=' + libname + ' ' +
  'BINNAME=' + libname

system('perl -Mlib=.. -MSigarBuild -e version_file ' + filters)

if is_win32
  system('perl -Mlib=.. -MSigarBuild -e resource_file ' + filters)
  system('rc /r sigar.rc')
  $LDFLAGS += ' sigar.res'
  $distcleanfiles << ['sigar.rc', 'sigar.res']
  #do not want dynamic runtime else "MSVCR80.dll was not found"
  $CFLAGS = $CFLAGS.gsub('-MD', '')
end

#XXX seems mkmf forces basename on srcs
#XXX should be linking against libsigar anyhow
(Dir["../../src/*.c"] + Dir["#{osdir}/*.c"] + Dir["#{osdir}/*.cpp"]).each do |file|
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
