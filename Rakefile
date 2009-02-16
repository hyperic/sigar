require 'rubygems'
require 'rake/gempackagetask'

props = {}
File.open("bindings/java/version.properties").each { |line|
  next if line =~ /^#/
  line.chomp!
  line.strip!
  next if line.empty?
  key,val = line.split('=')
  props[key] = val
}

GEM = props['project.name']
MAKE = (/mswin/ =~ RUBY_PLATFORM) ? 'nmake' : 'make'

spec = Gem::Specification.new do |s|
  s.name = GEM
  s.version = props['version.major'] + '.' + props['version.minor'] + '.' + props['version.maint']
  s.summary = props['project.summary']
  s.description = s.summary
  s.author = props['project.author']
  s.email = props['project.email']
  s.homepage = props['project.homepage']
  s.platform = Gem::Platform::RUBY
  s.has_rdoc = false
  s.extensions = 'bindings/ruby/extconf.rb'
  s.files =
    %w(COPYING EXCEPTIONS README Rakefile) +
    %w(bindings/SigarWrapper.pm) +
    Dir.glob("bindings/ruby/**/*") +
    Dir.glob("bindings/java/version.properties") +
    Dir.glob("include/*.h") +
    Dir.glob("src/**/*.[ch]")
end

Rake::GemPackageTask.new(spec) do |pkg|
  pkg.gem_spec = spec
end

task :make_spec do
  File.open("#{GEM}.gemspec", "w") do |file|
    file.puts spec.to_ruby
  end
end

def in_ext()
  ext = 'bindings/ruby'
  Dir.chdir(ext) if File.directory? ext
end

desc 'Build sigar extension'
task :build do
  in_ext();
  unless system("ruby extconf.rb")
    STDERR.puts "Failed to configure"
    break
  end
  unless system(MAKE)
    STDERR.puts 'Failed to ' + MAKE
    break
  end
end

desc 'Clean sigar extension'
task :clean do
  in_ext()
  system(MAKE + ' clean')
end

desc 'Dist Clean sigar extension'
task :distclean do
  in_ext()
  system(MAKE + ' distclean')
end

desc 'Run sigar examples (test)'
task :examples => [:build] do
  in_ext()
  Dir["examples/*.rb"].each do |file|
    cmd = "ruby #{file}"
    print cmd + "\n"
    system(cmd)
  end
end
