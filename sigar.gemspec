# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{sigar}
  s.version = "0.7.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Doug MacEachern"]
  s.date = %q{2010-01-17}
  s.description = %q{System Information Gatherer And Reporter}
  s.email = %q{sigar-users@hyperic.org}
  s.extensions = ["bindings/ruby/extconf.rb"]
  s.files = ["COPYING", "EXCEPTIONS", "README", "Rakefile", "version.properties", "bindings/SigarWrapper.pm", "bindings/ruby/darwin_sigar.c", "bindings/ruby/darwin_sigar.o", "bindings/ruby/examples", "bindings/ruby/examples/cpu_info.rb", "bindings/ruby/examples/df.rb", "bindings/ruby/examples/free.rb", "bindings/ruby/examples/ifconfig.rb", "bindings/ruby/examples/logging.rb", "bindings/ruby/examples/net_info.rb", "bindings/ruby/examples/netstat.rb", "bindings/ruby/examples/pargs.rb", "bindings/ruby/examples/penv.rb", "bindings/ruby/examples/route.rb", "bindings/ruby/examples/version.rb", "bindings/ruby/examples/who.rb", "bindings/ruby/extconf.rb", "bindings/ruby/Makefile", "bindings/ruby/pkg", "bindings/ruby/rbsigar.c", "bindings/ruby/rbsigar.o", "bindings/ruby/rbsigar_generated.rx", "bindings/ruby/sigar.bundle", "bindings/ruby/sigar.c", "bindings/ruby/sigar.o", "bindings/ruby/sigar_cache.c", "bindings/ruby/sigar_cache.o", "bindings/ruby/sigar_fileinfo.c", "bindings/ruby/sigar_fileinfo.o", "bindings/ruby/sigar_format.c", "bindings/ruby/sigar_format.o", "bindings/ruby/sigar_getline.c", "bindings/ruby/sigar_getline.o", "bindings/ruby/sigar_ptql.c", "bindings/ruby/sigar_ptql.o", "bindings/ruby/sigar_signal.c", "bindings/ruby/sigar_signal.o", "bindings/ruby/sigar_util.c", "bindings/ruby/sigar_util.o", "bindings/ruby/sigar_version.c", "bindings/ruby/sigar_version.o", "bindings/ruby/test", "bindings/ruby/test/cpu_test.rb", "bindings/ruby/test/file_system_test.rb", "bindings/ruby/test/helper.rb", "bindings/ruby/test/loadavg_test.rb", "bindings/ruby/test/mem_test.rb", "bindings/ruby/test/swap_test.rb", "bindings/ruby/test/uptime_test.rb", "include/sigar.h", "include/sigar_fileinfo.h", "include/sigar_format.h", "include/sigar_getline.h", "include/sigar_log.h", "include/sigar_private.h", "include/sigar_ptql.h", "include/sigar_util.h", "src/os/aix/aix_sigar.c", "src/os/aix/sigar_os.h", "src/os/darwin/darwin_sigar.c", "src/os/darwin/sigar_os.h", "src/os/hpux/dlpi.c", "src/os/hpux/hpux_sigar.c", "src/os/hpux/sigar_os.h", "src/os/linux/linux_sigar.c", "src/os/linux/sigar_os.h", "src/os/netware/netware_sigar.c", "src/os/netware/sigar_os.h", "src/os/osf1/osf1_sigar.c", "src/os/osf1/sigar_os.h", "src/os/solaris/get_mib2.c", "src/os/solaris/get_mib2.h", "src/os/solaris/hmekstat.h", "src/os/solaris/kstats.c", "src/os/solaris/procfs.c", "src/os/solaris/sigar_os.h", "src/os/solaris/solaris_sigar.c", "src/os/stub/sigar_os.h", "src/os/stub/stub_sigar.c", "src/os/win32/peb.c", "src/os/win32/sigar_os.h", "src/os/win32/sigar_pdh.h", "src/os/win32/win32_sigar.c", "src/sigar.c", "src/sigar_cache.c", "src/sigar_fileinfo.c", "src/sigar_format.c", "src/sigar_getline.c", "src/sigar_ptql.c", "src/sigar_signal.c", "src/sigar_util.c"]
  s.homepage = %q{http://sigar.hyperic.com/}
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.5}
  s.summary = %q{System Information Gatherer And Reporter}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
