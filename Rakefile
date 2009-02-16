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
