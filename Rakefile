require 'rubygems'
require 'rubygems/package_task'
require 'rake/testtask'

#so we can: ssh host rake -f $hudson_workspace/sigar/Rakefile
Dir.chdir(File.dirname(__FILE__))

props = {}
File.open("version.properties").each { |line|
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
#  s.version = props['version.major'] + '.' + props['version.minor'] + '.' + props['version.maint']
#  '0.7.x' until the sigar-1.7.0 release
  s.version = '0' + '.' + props['version.minor'] + '.' + '3'
  s.summary = props['project.summary']
  s.description = s.summary
  s.author = props['project.author']
  s.email = props['project.email']
  s.homepage = props['project.homepage']
  s.platform = Gem::Platform::RUBY
  s.has_rdoc = false
  s.extensions = 'bindings/ruby/extconf.rb'
  s.files =
    %w(LICENSE NOTICE README Rakefile version.properties) +
    %w(bindings/SigarWrapper.pm bindings/SigarBuild.pm) +
    `git ls-files -- bindings/ruby/*.*`.split("\n") +
    Dir.glob("include/*.h") +
    Dir.glob("src/**/*.[ch]") +
    Dir.glob("src/**/*.in")
end

Gem::PackageTask.new(spec) do |pkg|
  pkg.gem_spec = spec
end

task :default => :test

def in_ext()
  ext = 'bindings/ruby'
  Dir.chdir(ext) if File.directory? ext
end

desc 'Build sigar extension'
task :build do
  in_ext()
  unless File.exists? "Makefile"
    unless system("ruby extconf.rb")
      STDERR.puts "Failed to configure"
      break
    end
  end
  unless system(MAKE)
    STDERR.puts 'Failed to ' + MAKE
    break
  end
end

Rake::TestTask.new do |t|
  t.pattern = 'test/*_test.rb'
  t.libs << "."
end

task :test => [:build] do
  in_ext()
end

desc 'Clean sigar extension'
task :clean do
  in_ext()
  system(MAKE + ' clean') if File.exists? "Makefile"
end

desc 'Dist Clean sigar extension'
task :distclean do
  in_ext()
  system(MAKE + ' distclean') if File.exists? "Makefile"
end

desc 'Run sigar examples (test)'
task :examples => [:build] do
  in_ext()
  Dir["examples/*.rb"].each do |file|
    cmd = "ruby -I. #{file}"
    print cmd + "\n"
    system(cmd)
  end
end

desc "create a gemspec file"
task :make_spec do
  File.open("#{GEM}.gemspec", "w") do |file|
    file.puts spec.to_ruby
  end
end
