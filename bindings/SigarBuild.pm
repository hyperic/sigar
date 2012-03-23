#
# Copyright (c) 2009 Hyperic, Inc.
# Copyright (c) 2009 SpringSource, Inc.
# Copyright (c) 2009 VMware, Inc.
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

package SigarBuild;

use strict;
use Config;
use Exporter;
use File::Basename qw(basename);
use File::Copy qw(copy);
use File::Spec ();
use POSIX ();

use vars qw(@ISA @EXPORT);
@ISA = qw(Exporter);
@EXPORT = qw(cppflags ldflags libs os src inline_src version_file resource_file);

sub archname {
    my $os = lc $^O;
    my $vers = $Config{osvers};
    my $arch = $Config{archname};

    if ($os =~ /win32/) {
        return 'x86-winnt';
    }
    elsif ($os =~ /linux/) {
        if ($arch =~ /_64/) {
            return 'amd64-linux';
        }
        else {
            return 'x86-linux';
        }
    }
    elsif ($os =~ /hpux/) {
        if ($vers =~ /11\./) {
            return 'pa-hpux-11';
        }
    }
    elsif ($os =~ /aix/) {
        return 'ppc-aix-5';
    }
    elsif ($os =~ /solaris/) {
        if ($arch =~ /sun4/) {
            return 'sparc-solaris';
        }
        elsif ($arch =~ /.86/) {
            return 'x86-solaris';
        }
    }
    elsif ($os =~ /darwin/) {
        return 'universal-macosx';
    }
    elsif ($os =~ /freebsd/) {
        if ($arch =~ /.86/) {
            if($vers =~ /6\../ ) {
                return 'x86-freebsd-6';
            }
        }
        elsif ($arch =~ /amd64/) {
            if ($vers =~ /6\../ ) {
                return 'amd64-freebsd-6';
            }
        }
    }

    return '';
}

sub flags {
    my $os = lc $^O;
    my $is_win32 = 0;
    my (@cppflags, @ldflags, @libs);
    if ($os =~ /(win32)/) {
        $os = $1;
        $is_win32 = 1;
        @cppflags = ('-DWIN32', '-D_CRT_SECURE_NO_DEPRECATE');
        @libs = qw(kernel32 user32 advapi32 ws2_32 netapi32 shell32 pdh version comsupp wbemuuid);
    }
    elsif ($os =~ /(linux)/) {
        $os = $1;
    }
    elsif ($os =~ /(hpux)/) {
        $os = $1;
        @libs = qw(nsl nm);
    }
    elsif ($os =~ /(aix)/) {
        $os = $1;
        @libs = qw(odm cfg perfstat);
    }
    elsif ($os =~ /(solaris)/) {
        $os = $1;
        @libs = qw(nsl socket kstat);
    }
    elsif ($os =~ /(darwin)/) {
        $os = $1;
        @cppflags = ('-DDARWIN');
        @ldflags = ('-framework CoreServices', '-framework IOKit');
        if (-e "/usr/local/libproc.h") {
            push @cppflags, '-DDARWIN_HAS_LIBPROC_H';
        }
    }
    elsif ($os =~ /bsd/) {
        $os = 'darwin';
        @libs = qw(kvm);
    }

    push @cppflags,
      '-I../../include',
      "-I../../src/os/$os";

    unless ($is_win32) {
        push @cppflags, '-U_FILE_OFFSET_BITS';
    }

    my(@src) = (<../../src/*.c>, <../../src/os/$os/*.c>, <../../src/os/$os/*.cpp>);

    return {
        is_win32 => $is_win32,
        os => $os,
        libs => \@libs,
        cppflags => \@cppflags,
        ldflags => \@ldflags,
        src => \@src,
    };
}

#perl -Mlib=.. -MSigarBuild -e cppflags
sub cppflags {
    print join ' ', @{ flags()->{cppflags} };
}

sub ldflags {
    print join ' ', @{ flags()->{ldflags} };
}

sub libs {
    print join ' ', @{ flags()->{libs} };
}

sub os {
    print flags()->{os};
}

sub src {
    print join ' ', @{ flags()->{src} };
}

sub inline_src {
    my $stdout = @_ ? 0 : 1;
    my $flags = shift || flags();
    my $src = $flags->{src};
    my $dir = $flags->{build_dir} || $ARGV[0];
    my(@files);
    #unlink symlinks incase of nfs shared dir...
    for my $file (grep { -l } <*.c>) {
        unlink $file;
    }
    for my $file (@$src) {
        my $cf = basename $file;
        #sigar.c -> libsigar.c else
        #sigar.o and perl Sigar.o clash on case insensitive filesystems
        $cf = 'libsigar.c' if $cf eq 'sigar.c';
        if ($dir) {
            $cf = join '/', $dir, $cf;
            $file = File::Spec->rel2abs($file);
        }
        push @files, $cf;
        if ($flags->{is_win32}) {
            copy($file, $cf);
        }
        else {
            symlink($file, $cf) unless -e $cf;
        }
    }
    if ($stdout) {
        print join ' ', @files;
    }
    else {
        return @files;
    }
}

sub scm_revision {
    my $rev;
    $rev = `git rev-parse --short HEAD`;
    if ($rev) {
        chomp $rev;
    }
    else {
        $rev = "exported";
    }
    return $rev;
}

sub build_date {
    return POSIX::strftime("%m/%d/%Y %I:%M %p", localtime);
}

sub find_file {
    my $file = shift;
    for my $dir (qw(../.. .. .)) {
        my $pfile = "$dir/$file";
        return $pfile if -e $pfile;
    }
    return $file;
}

sub version_properties {
    my $props = {};
    my $file = $_[0] || find_file('version.properties');
    open my $fh, $file or die "open $file: $!";
    while (<$fh>) {
        chomp;
        my($key,$val) = split '=';
        next unless $key and defined $val;
        $props->{$key} = $val;
    }
    close $fh;

    $props->{'scm.revision'} = scm_revision();

    $props->{'build.date'} = build_date();

    $props->{'version'} =
        join '.', map $props->{"version.$_"}, qw(major minor maint);

    $props->{'version.build'} = $ENV{BUILD_NUMBER} || '0';

    $props->{'version.string'} =
        join '.', $props->{'version'}, $props->{'version.build'};

    return $props;
}

sub resource_file {
    my(@args) = @_ ? @_ : @ARGV;
    version_file(find_file("src/os/win32/sigar.rc.in"), "sigar.rc", @args);
}

sub version_file {
    local $_;
    my($source, $dest, %filters);
    my(@args) = @_ ? @_ : @ARGV;
    for (@args) {
        if (/=/) {
            my($key,$val) = split '=', $_, 2;
            $filters{$key} = $val;
        }
        else {
            if ($source) {
                $dest = $_;
            }
            else {
                $source = $_;
            }
        }
    }
    unless ($source) {
        $dest = 'sigar_version.c';
        $source = find_file("src/$dest.in");
    }
    my $props = version_properties();
    while (my($key,$val) = each %$props) {
        $key = uc $key;
        $key =~ s/\./_/;
        $filters{$key} = $val;
    }
    my $re = join '|', keys %filters;
    open my $in, $source or die "open $source: $!";
    my $out;
    if ($dest) {
        open $out, '>', $dest or die "open $dest: $!";
    }
    else {
        $out = \*STDOUT;
    }
    while (<$in>) {
        s/\@\@($re)\@\@/$filters{$1}/go;
        print $out $_;
    }
    close $in;
    close $out if $dest;
}

1;
__END__
