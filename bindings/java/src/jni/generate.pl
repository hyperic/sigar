use strict;
use File::Path;

my %platforms = (
    A => "AIX",
    D => "Darwin",
    F => "FreeBSD",
    H => "HPUX",
    L => "Linux",
    S => "Solaris",
    W => "Win32",
);

sub supported_platforms {
    my $p = shift;
    return 'Undocumented' unless $p;
    if ($p eq '*') {
        return 'All';
    }

    my @platforms;
    for (split //, $p) {
        push @platforms, $platforms{$_};
    }

    return join ", ", @platforms;
}

#this script generates jni code and java classes for the following table

my %classes = (
    Mem => [
      {
         name => 'total', type => 'Long',
         desc => 'Total system memory',
         plat => '*',
         cmd  => {
             AIX => 'lsattr -El sys0 -a realmem',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => '',
             Win32   => 'taskman',
         },
      },
      {
         name => 'ram', type => 'Long',
         desc => 'System Random Access Memory (in MB)',
         plat => '*',
         cmd  => {
             AIX     => 'lsattr -El sys0 -a realmem',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'cat /proc/mtrr | head -1',
             Solaris => '',
             Win32   => '',
         },
      },
      {
         name => 'used', type => 'Long',
         desc => 'Total used system memory',
         plat => '*',
         cmd  => {
             AIX     => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => '',
             Win32   => 'taskman',
         },
      },
      {
         name => 'free', type => 'Long',
         desc => 'Total free system memory (e.g. Linux plus cached)',
         plat => '*',
         cmd  => {
             AIX     => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => '',
             Win32   => 'taskman',
         },
      },
      {
         name => 'actual_used', type => 'Long',
         desc => 'Actual total used system memory (e.g. Linux minus buffers)',
         plat => '*',
         cmd  => {
             AIX     => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => '',
             Win32   => 'taskman',
         },
      },
      {
         name => 'actual_free', type => 'Long',
         desc => 'Actual total free system memory',
         plat => '*',
         cmd  => {
             AIX     => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => '',
             Win32   => 'taskman',
         },
      },
      {
         name => 'shared', type => 'Long',
         desc => 'Total shared system memory',
         plat => 'LSW',
         cmd  => {
             AIX     => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => '',
             Win32   => '',
         },
      },
    ],
    Swap => [
      {
         name => 'total', type => 'Long',
         desc => 'Total system swap',
         plat => '*',
         cmd  => {
             AIX     => 'lsps -s',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => 'swap -s',
             Win32   => '',
         },
      },
      {
         name => 'used', type => 'Long',
         desc => 'Total used system swap',
         plat => '*',
         cmd  => {
             AIX     => 'lsps -s',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => 'swap -s',
             Win32   => '',
         },
      },
      {
         name => 'free', type => 'Long',
         desc => 'Total free system swap',
         plat => '*',
         cmd  => {
             AIX => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'free',
             Solaris => 'swap -s',
             Win32   => '',
         },
      },
    ],
    Cpu => [
      {
         name => 'user', type => 'Long',
         desc => 'Total system cpu user time',
         plat => '*'
      },
      {
         name => 'sys', type => 'Long',
         desc => 'Total system cpu kernel time',
         plat => '*'
      },
      {
         name => 'nice', type => 'Long',
         desc => 'Total system cpu nice time',
         plat => 'DFHL'
      },
      {
         name => 'idle', type => 'Long',
         desc => 'Total system cpu idle time',
         plat => '*'
      },
      {
         name => 'wait', type => 'Long',
         desc => 'Total system cpu io wait time',
         plat => 'ALHS'
      },
      {
         name => 'total', type => 'Long',
         desc => 'Total system cpu time',
         plat => '*'
      },
    ],
    CpuInfo => [
      {
         name => 'vendor', type => 'String',
         desc => 'CPU vendor id',
         plat => 'AFLHSW'
      },
      {
         name => 'model', type => 'String',
         desc => 'CPU model',
         plat => 'AFLHSW'
      },
      {
         name => 'mhz', type => 'Int',
         desc => 'CPU speed',
         plat => 'AFHLSW'
      },
      {
         name => 'cache_size', type => 'Long',
         desc => 'CPU cache size',
         plat => 'AL'
      },
    ],
    Uptime => [
      {
         name => 'uptime', type => 'Double',
         desc => 'Time since machine started in seconds',
         plat => '*'
      },
    ],
    ProcMem => [
      {
         name => 'size', type => 'Long',
         desc => 'Total process memory',
         plat => 'AFHLSW'
      },
      {
         name => 'vsize', type => 'Long',
         desc => 'Total process virtual memory',
         plat => 'AFHLSW'
      },
      {
         name => 'resident', type => 'Long',
         desc => 'Total process non-swapped memory',
         plat => 'L'
      },
      {
         name => 'share', type => 'Long',
         desc => 'Total process shared memory',
         plat => 'AHLS'
      },
      {
         name => 'rss', type => 'Long',
         desc => 'Process resident set size',
         plat => 'AFHL'
      },
    ],
    ProcCred => [
      {
         name => 'uid', type => 'Long',
         desc => 'Process user id',
         plat => 'ADFHLS'
      },
      {
         name => 'gid', type => 'Long',
         desc => 'Process group id',
         plat => 'ADFHLS'
      },
      {
         name => 'euid', type => 'Long',
         desc => 'Process effective user id',
         plat => 'ADFHLS'
      },
      {
         name => 'egid', type => 'Long',
         desc => 'Process effective group id',
         plat => 'ADFHLS'
      },
    ],
    ProcCredName => [
      {
         name => 'user', type => 'String',
         desc => 'Process owner user name',
         plat => '*'
      },
      {
         name => 'group', type => 'String',
         desc => 'Process owner group name',
         plat => '*'
      },
    ],
    ProcTime => [
      {
         name => 'start_time', type => 'Long',
         desc => 'Time process was started in seconds',
         plat => '*'
      },
      {
         name => 'user', type => 'Long',
         desc => 'Process cpu user time',
         plat => '*'
      },
      {
         name => 'sys', type => 'Long',
         desc => 'Process cpu kernel time',
         plat => '*'
      },
      {
         name => 'total', type => 'Long',
         desc => 'Process cpu time (sum of User and Sys)',
         plat => '*'
      },
    ],
    ProcState => [
      {
         name => 'state', type => 'Char',
         desc => 'Process state (Running, Zombie, etc.)',
         plat => '*'
      },
      {
         name => 'name', type => 'String',
         desc => 'Name of the process program',
         plat => '*'
      },
      {
         name => 'ppid', type => 'Long',
         desc => 'Process parent process id',
         plat => '*'
      },
      {
         name => 'tty', type => 'Int',
         desc => 'Device number of rocess controling terminal',
         plat => 'AHLS'
      },
      {
         name => 'nice', type => 'Int',
         desc => 'Nice value of process',
         plat => 'ADFHLS'
      },
      {
         name => 'priority', type => 'Int',
         desc => 'Kernel scheduling priority of process',
         plat => 'DFHLSW'
      },
    ],
    ProcFd => [
      {
         name => 'total', type => 'Long',
         desc => 'Total number of open file descriptors',
         plat => 'AHLSW'
      },
    ],
    ProcStat => [
      {
         name => 'total', type => 'Long',
         desc => 'Total number of processes',
         plat => '*'
      },
    ],
    ProcExe => [
      {
         name => 'name', type => 'String',
         desc => 'Name of process executable',
         plat => 'FLSW',
         cmd  => {
             AIX => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'ls -l /proc/$$/exe',
             Solaris => '',
             Win32   => '',
         },
      },
      {
         name => 'cwd', type => 'String',
         desc => 'Name of process current working directory',
         plat => 'LSW',
         cmd  => {
             AIX => '',
             Darwin  => '',
             FreeBSD => '',
             HPUX    => '',
             Linux   => 'ls -l /proc/$$/cwd',
             Solaris => '',
             Win32   => '',
         },
      },
    ],
    ThreadCpu => [
      {
         name => 'user', type => 'Long',
         desc => 'Thread cpu user time',
         plat => 'AHLSW'
      },
      {
         name => 'sys', type => 'Long',
         desc => 'Thread cpu kernel time',
         plat => 'AHLSW'
      },
      {
         name => 'total', type => 'Long',
         desc => 'Thread cpu time (sum of User and Sys)',
         plat => 'AHLSW'
      },
    ],
    FileSystem => [
      {
         name => 'dir_name', type => 'String',
         desc => 'Directory name',
         plat => '*'
      },
      {
         name => 'dev_name', type => 'String',
         desc => 'Device name',
         plat => '*'
      },
      {
         name => 'type_name', type => 'String',
         desc => 'File system generic type name',
         plat => '*'
      },
      {
         name => 'sys_type_name', type => 'String',
         desc => 'File system os specific type name',
         plat => '*'
      },
      {
         name => 'type', type => 'Int',
         desc => 'File system type',
         plat => '*'
      },
      {
         name => 'flags', type => 'Long',
         desc => 'File system flags',
         plat => '*'
      },
    ],
    FileSystemUsage => [
      {
         name => 'total', type => 'Long',
         desc => 'Total bytes of filesystem',
         plat => '*'
      },
      {
         name => 'free', type => 'Long',
         desc => 'Total free bytes on filesytem',
         plat => '*'
      },
      {
         name => 'avail', type => 'Long',
         desc => 'Total free bytes on filesytem available to caller',
         plat => '*'
      },
      {
         name => 'files', type => 'Long',
         desc => 'Total number of file nodes on the filesystem',
         plat => 'ADFHLS'
      },
      {
         name => 'free_files', type => 'Long',
         desc => 'Number of free file nodes on the filesystem',
         plat => 'ADFHLS'
      },
      {
         name => 'disk_reads', type => 'Long',
         desc => 'Number of physical disk reads',
         plat => 'AHLSW'
      },
      {
         name => 'disk_writes', type => 'Long',
         desc => 'Number of physical disk writes',
         plat => 'AHLSW'
      },
      {
         name => 'use_percent', type => 'Double',
         desc => 'Percent of disk used',
         plat => '*'
      },
    ],
    FileAttrs => [
      {
         name => 'permissions', type => 'Long',
      },
      {
         name => 'type', type => 'Int',
      },
      {
         name => 'uid', type => 'Long',
      },
      {
         name => 'gid', type => 'Long',
      },
      {
         name => 'inode', type => 'Long',
      },
      {
         name => 'device', type => 'Long',
      },
      {
         name => 'nlink', type => 'Long',
      },
      {
         name => 'size', type => 'Long',
      },
      {
         name => 'atime', type => 'Long',
      },
      {
         name => 'ctime', type => 'Long',
      },
      {
         name => 'mtime', type => 'Long',
      },
    ],
    DirStat => [
      {
         name => 'total', type => 'Long',
      },
      {
         name => 'files', type => 'Long',
      },
      {
         name => 'subdirs', type => 'Long',
      },
      {
         name => 'symlinks', type => 'Long',
      },
      {
         name => 'chrdevs', type => 'Long',
      },
      {
         name => 'blkdevs', type => 'Long',
      },
      {
         name => 'sockets', type => 'Long',
      },
    ],
    NetRoute => [
      {
         name => 'destination', type => 'NetAddr',
         desc => '',
         plat => 'HLW'
      },
      {
         name => 'gateway', type => 'NetAddr',
         desc => '',
         plat => 'HLW'
      },
      {
         name => 'flags', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'refcnt', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'use', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'metric', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'mask', type => 'NetAddr',
         desc => '',
         plat => 'HL'
      },
      {
         name => 'mtu', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'window', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'irtt', type => 'Long',
         desc => '',
         plat => 'L'
      },
      {
         name => 'ifname', type => 'String',
         desc => '',
         plat => 'L'
      },
    ],
    NetInterfaceConfig => [
      {
         name => 'name', type => 'String',
         desc => '',
         plat => '*'
      },
      {
         name => 'hwaddr', type => 'String',
         desc => '',
         plat => '*'
      },
      {
         name => 'address', type => 'NetAddr',
         desc => '',
         plat => '*'
      },
      {
         name => 'destination', type => 'NetAddr',
         desc => '',
         plat => '*'
      },
      {
         name => 'broadcast', type => 'NetAddr',
         desc => '',
         plat => '*'
      },
      {
         name => 'netmask', type => 'NetAddr',
         desc => '',
         plat => '*'
      },
      {
         name => 'flags', type => 'Long',
         desc => '',
         plat => '*'
      },
      {
         name => 'mtu', type => 'Long',
         desc => '',
         plat => 'DFL'
      },
      {
         name => 'metric', type => 'Long',
         desc => '',
         plat => 'DFL'
      },
    ],
    NetInterfaceStat => [
      {
         name => 'rx_bytes', type => 'Long',
         desc => '',
         plat => '*'
      },
      {
         name => 'rx_packets', type => 'Long',
         desc => '',
         plat => '*'
      },
      {
         name => 'rx_errors', type => 'Long',
         desc => '',
         plat => '*'
      },
      {
         name => 'rx_dropped', type => 'Long',
         desc => '',
         plat => ''
      },
      {
         name => 'rx_overruns', type => 'Long',
         desc => '',
         plat => ''
      },
      {
         name => 'rx_frame', type => 'Long',
         desc => '',
         plat => ''
      },
      {
         name => 'tx_bytes', type => 'Long',
         desc => '',
         plat => '*'
      },
      {
         name => 'tx_packets', type => 'Long',
         desc => '',
         plat => '*'
      },
      {
         name => 'tx_errors', type => 'Long',
         desc => '*',
         plat => ''
      },
      {
         name => 'tx_dropped', type => 'Long',
         desc => '',
         plat => ''
      },
      {
         name => 'tx_overruns', type => 'Long',
         desc => '',
         plat => ''
      },
      {
         name => 'tx_collisions', type => 'Long',
         desc => '',
         plat => ''
      },
      {
         name => 'tx_carrier', type => 'Long',
         desc => '',
         plat => ''
      },
    ],
    NetConnection => [
      {
         name => 'local_port', type => 'Long',
         desc => '',
         plat => 'LSW'
      },
      {
         name => 'local_address', type => 'String',
         desc => '',
         plat => 'LSW'
      },
      {
         name => 'remote_port', type => 'Long',
         desc => '',
         plat => 'LSW'
      },
      {
         name => 'remote_address', type => 'String',
         desc => '',
         plat => 'LSW'
      },
      {
         name => 'type', type => 'Int',
         desc => '',
         plat => 'LSW'
      },
      {
         name => 'state', type => 'Int',
         desc => '',
         plat => 'LSW'
      },
      {
         name => 'send_queue', type => 'Long',
         desc => '',
         plat => 'LS'
      },
      {
         name => 'receive_queue', type => 'Long',
         desc => '',
         plat => 'LS'
      },
    ],
    Who => [
      {
         name => 'user', type => 'String',
         desc => '',
         plat => ''
      },
      {
         name => 'device', type => 'String',
         desc => '',
         plat => ''
      },
      {
         name => 'host', type => 'String',
         desc => '',
         plat => ''
      },
      {
         name => 'time', type => 'Long',
         desc => '',
         plat => ''
      },
    ],
);

my %cmds = (
    Mem => {
       AIX     => 'top',
       Darwin  => 'top',
       FreeBSD => 'top',
       HPUX    => 'top',
       Linux   => 'top',
       Solaris => 'top',
       Win32   => 'taskman',
    },
    Swap => {
       AIX     => 'top',
       Darwin  => 'top',
       FreeBSD => 'top',
       HPUX    => 'top',
       Linux   => 'top',
       Solaris => 'top',
       Win32   => 'taskman',
    },
    Cpu => {
       AIX     => 'top',
       Darwin  => 'top',
       FreeBSD => 'top',
       HPUX    => 'top',
       Linux   => 'top',
       Solaris => 'top',
       Win32   => 'taskman',
    },
    CpuInfo => {
       AIX     => 'lsattr -El proc0',
       Darwin  => '',
       FreeBSD => '',
       HPUX    => '',
       Linux   => 'cat /proc/cpuinfo',
       Solaris => 'psrinfo -v',
       Win32   => '',
    },
    Uptime => {
       AIX     => 'uptime',
       Darwin  => 'uptime',
       FreeBSD => 'uptime',
       HPUX    => 'uptime',
       Linux   => 'uptime',
       Solaris => 'uptime',
       Win32   => '',
    },
    ProcMem => {
       AIX     => 'top, ps',
       Darwin  => 'top, ps',
       FreeBSD => 'top, ps',
       HPUX    => 'top, ps',
       Linux   => 'top, ps',
       Solaris => 'top, ps',
       Win32   => 'taskman',
    },
    ProcCred => {
       AIX     => 'top, ps',
       Darwin  => 'top, ps',
       FreeBSD => 'top, ps',
       HPUX    => 'top, ps',
       Linux   => 'top, ps',
       Solaris => 'top, ps',
       Win32   => 'taskman',
    },
    ProcTime => {
       AIX     => 'top, ps',
       Darwin  => 'top, ps',
       FreeBSD => 'top, ps',
       HPUX    => 'top, ps',
       Linux   => 'top, ps',
       Solaris => 'top, ps',
       Win32   => 'taskman',
    },
    ProcState => {
       AIX     => 'top, ps',
       Darwin  => 'top, ps',
       FreeBSD => 'top, ps',
       HPUX    => 'top, ps',
       Linux   => 'top, ps',
       Solaris => 'top, ps',
       Win32   => 'taskman',
    },
    ProcFd => {
       AIX     => 'lsof',
       Darwin  => 'lsof',
       FreeBSD => 'lsof',
       HPUX    => 'lsof',
       Linux   => 'lsof',
       Solaris => 'lsof',
       Win32   => '',
    },
    ProcStat => {
       AIX     => 'top, ps',
       Darwin  => 'top, ps',
       FreeBSD => 'top, ps',
       HPUX    => 'top, ps',
       Linux   => 'top, ps',
       Solaris => 'top, ps',
       Win32   => 'taskman',
    },
    FileSystemUsage => {
       AIX     => 'df',
       Darwin  => 'df',
       FreeBSD => 'df',
       HPUX    => 'df',
       Linux   => 'df',
       Solaris => 'df',
       Win32   => '',
    },
    NetRoute => {
       AIX     => '',
       Darwin  => '',
       FreeBSD => '',
       HPUX    => '',
       Linux   => 'route -n',
       Solaris => '',
       Win32   => '',
    },
    NetInterfaceConfig => {
       AIX     => '',
       Darwin  => '',
       FreeBSD => '',
       HPUX    => '',
       Linux   => 'ifconfig',
       Solaris => 'ifconfig -a',
       Win32   => '',
    },
    NetInterfaceStat => {
       AIX     => '',
       Darwin  => '',
       FreeBSD => '',
       HPUX    => '/usr/sbin/lanadmin -g mibstats 0, netstat -i',
       Linux   => 'ifconfig',
       Solaris => '',
       Win32   => '',
    },
    NetConnection => {
       AIX     => '',
       Darwin  => '',
       FreeBSD => '',
       HPUX    => '',
       Linux   => 'netstat',
       Solaris => '',
       Win32   => '',
    },
);

my %jfields = (
    Long   => "J",
    Double => "D",
    Int    => "I",
    Char   => "C",
    String => "Ljava/lang/String;",
);

my %pfields = (
    Long   => "UV",
    Double => "double",
    Int    => "IV",
    Char   => "char",
    String => "char *",
    NetAddr => "Sigar::NetAddr",
);

$jfields{'NetAddr'} = $jfields{'String'};

my %jinit = (
    String => 'null',
);

my %jtype = (
    String  => 'String',
);

#alias
for my $j (\%jfields, \%jinit, \%jtype) {
    $j->{'NetAddr'} = $j->{'String'};
}

my %func_alias = (
);

my $cfile = 'javasigar_generated.c';
my $hfile = 'javasigar_generated.h';
my $pfile = 'Sigar_generated.xs';

if ((stat $0)[9] < (stat "src/jni/$cfile")[9]) {
    print "$cfile unchanged\n";
    exit;
}

print "generating $cfile\n";

my $build_src = $ARGV[0] or die "usage: $0 build_directory";

if (! -d $build_src) {
    die "$build_src: $!";
}

chdir $build_src;

my $jsrc = 'net/hyperic/sigar';
mkpath([$jsrc], 0, 0755) unless -d $jsrc;

open CFH, ">$cfile" or die "open $cfile: $!";
open HFH, ">$hfile" or die "open $hfile: $!";
open PFH, ">$pfile" or die "open $pfile: $!";

my $datestamp = scalar localtime;

my $warning = <<EOF;

/*****************************************************
 * WARNING: this file was generated by $0
 * on $datestamp, any changes
 * made here will be lost.
 *****************************************************/

EOF

print CFH $warning;
print HFH $warning;

#XXX kinda ugly having this here
#will consider moving elsewhere if there
#are more cases like this.
my %extra_code = (
    FileSystem => <<'EOF',
    public static final int TYPE_UNKNOWN    = 0;
    public static final int TYPE_NONE       = 1;
    public static final int TYPE_LOCAL_DISK = 2;
    public static final int TYPE_NETWORK    = 3;
    public static final int TYPE_RAM_DISK   = 4;
    public static final int TYPE_CDROM      = 5;
    public static final int TYPE_SWAP       = 6;
EOF
    NetConnection => <<'EOF',
    public native String getTypeString();

    public native String getStateString();
EOF
    Mem => <<'EOF',
    public String toString() {
        return
            "Mem: " +
            (this.total / 1024) + "K av, " +
            (this.used / 1024) + "K used, " +
            (this.free / 1024) + "K free, " +
            (this.shared / 1024) + "K shrd";
    }
EOF
    Swap => <<'EOF',
    public String toString() {
        return
            "Swap: " +
            (this.total / 1024) + "K av, " +
            (this.used / 1024) + "K used, " +
            (this.free / 1024) + "K free";
    }
EOF
    ProcState => <<'EOF',
    public static final char SLEEP  = 'S';
    public static final char RUN    = 'R';
    public static final char STOP   = 'T';
    public static final char ZOMBIE = 'Z';
    public static final char IDLE   = 'D';
EOF
);

my %has_name_arg = map { $_, 1 } qw(FileSystemUsage FileAttrs DirStat
                                    NetInterfaceConfig NetInterfaceStat);
my %proc_no_arg = map { $_, 1 } qw(stat);
my %get_not_impl = map { $_, 1 } qw(net_route net_connection who
                                    cpu_info file_system); #list funcs only

my %field_cache;
my $i = 0;
while (my($class, $fields) = each %classes) {
    next if $field_cache{$class}++;
    print HFH "#define JSIGAR_FIELDS_\U$class $i\n";
    $i++;
    my $n = 0;
    for my $field (@$fields) {
        my $name = $field->{name};
        print HFH "#   define JSIGAR_FIELDS_\U${class}_${name} $n\n";
        $n++;
    }
    print HFH "#   define JSIGAR_FIELDS_\U${class}_MAX $n\n";
}
print HFH "#define JSIGAR_FIELDS_MAX $i\n";

while (my($name, $fields) = each %classes) {
    my $java_class = "net.hyperic.sigar.$name";
    (my $jni_prefix = "Java.$java_class") =~ s/\./_/g;
    my $class = $name;
    my $cname;
    my $args_proto = "";
    my($arg, $arg_type);
    my $args = "";
    my $is_proc = 0;
    my $decl_string = "";
    my $get_string = "";
    my $release_string = "";

    my $jname = lcfirst $name;

    #example: FileSystemUsage -> file_system_usage
    ($cname = $name) =~ s/([a-z])([A-Z])/$1_$2/g;
    $cname = lc $cname;

    if ($cname =~ /^proc_(\w+)/ or $cname =~ /^thread_cpu/) {
        unless ($proc_no_arg{$1}) {
            $args_proto = ", jlong pid";
            $arg_type = 'sigar_pid_t';
            $arg  = 'pid';
            $args = " $arg, ";
            $is_proc = 1;
        }
    }
    elsif ($has_name_arg{$name}) {
        #hallo freulien
        $args_proto = ", jstring jname";
        $arg_type = 'const char *';
        $decl_string = "const char *name;";
        $get_string = "name = JENV->GetStringUTFChars(env, jname, 0);";
        $release_string = "JENV->ReleaseStringUTFChars(env, jname, name);";
        $arg  = 'name';
        $args = " $arg, ";
    }

    my $sigar_prefix = join '_', 'sigar', $cname;

    my $sigar_function = join '_', $sigar_prefix, 'get';
    $sigar_function = $func_alias{$sigar_function} || $sigar_function;
    my $sigar_type = join '_', $sigar_prefix, 't';

    my $nativefunc = join '_', $jni_prefix, 'gather';

    my $proto = join "\n",
      "JNIEXPORT void JNICALL $nativefunc",
        "(JNIEnv *env, jobject obj, jobject sigar_obj$args_proto)";

    my $jfile = "$name.java";
    open JFH, ">$jsrc/$jfile" or die "open $jfile: $!";
    print JFH $warning;

    my $impl = ! $get_not_impl{$cname};

    print CFH <<EOF if $impl;

$proto;

$proto
{
    $sigar_type s;
    int status;
    jclass cls = JENV->GetObjectClass(env, obj);
    $decl_string
    dSIGAR_VOID;

    $get_string

    status = $sigar_function(sigar,${args}&s);

    $release_string

    if (status != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return;
    }

EOF

    my $jargs_proto = 'Sigar sigar';
    my $jargs = 'sigar';

    if ($is_proc) {
        $jargs_proto .= ', long pid';
        $jargs .= ", pid";
    }
    elsif ($has_name_arg{$name}) {
        $jargs_proto .= ', String name';
        $jargs .= ", name";
    }

    my $cache_field_ids = 1;

    print JFH <<EOF;
package net.hyperic.sigar;

/**
 * $name sigar class.
 */
public class $name {

    public $name() { }

    public native void gather($jargs_proto) throws SigarException;

    /**
     * This method is not intended to be called directly.
     * use Sigar.get$name() instead.
     * \@exception SigarException on failure.
     * \@see net.hyperic.sigar.Sigar#get$name
     */
    static $name fetch($jargs_proto) throws SigarException {
        $name $jname = new $name();
        $jname.gather($jargs);
        return $jname;
    }

EOF

    my @copy;
    my $define = "JAVA_SIGAR_SET_FIELDS_\U$name";
    my @macro = ("\#define $define(cls, obj, s)");
    my $init_define = "JAVA_SIGAR_INIT_FIELDS_\U$name";
    my $field_class_ix = "JSIGAR_FIELDS_\U$name";
    my $field_class_ix = "JSIGAR_FIELDS_\U$name";
    my $field_class_max = $field_class_ix . '_MAX';
    my $field_class = "jsigar->fields[$field_class_ix]";

    my @init_fields = ("#define $init_define(cls)",
                       "    if (!$field_class) {",
                       "        $field_class = ",
                       "            malloc(sizeof(*$field_class));",
                       "        $field_class->classref = ",
                       "            (jclass)JENV->NewGlobalRef(env, cls);",
                       "        $field_class->ids = ",
                       "            malloc($field_class_max *",
                       "                   sizeof(*$field_class->ids));");

    my $perl_class = "Sigar::$class";
    if (0) {
        #insert into Sigar.xs
        (my $perl_typedef = $perl_class) =~ s/:/_/g;
        print "typedef $sigar_type * $perl_typedef;\n";
    }
    elsif (0) {
        #insert into typemap
        print "$perl_class T_PTROBJ\n";
    }
    
    print PFH "\nMODULE = Sigar   PACKAGE = Sigar   PREFIX = sigar_\n\n";

    my $xs_args = 'sigar';
    if ($arg) {
        $xs_args .= ", $arg";
    }

    print PFH <<EOF if $impl;
$perl_class
$cname($xs_args)
    Sigar sigar
EOF
    if ($arg) {
        print PFH "    $arg_type $arg\n" if $impl;
    }

    print PFH <<EOF if $impl;

    PREINIT:
    int status;

    CODE:
    RETVAL = safemalloc(sizeof(*RETVAL));
    if ((status = $sigar_function($xs_args, RETVAL)) != SIGAR_OK) {
        SIGAR_CROAK(sigar, "$cname");
    }

    OUTPUT:
    RETVAL
EOF

  print PFH <<EOF;

MODULE = Sigar   PACKAGE = $perl_class   PREFIX = sigar_

void
DESTROY(obj)
    $perl_class obj

    CODE:
    safefree(obj);

EOF

    for my $field (@$fields) {
        my $type = $field->{type};
        my $name = $field->{name};
        my $desc = $field->{desc} || $name;
        (my $jname = $name) =~ s/_(\w)/\u$1/g;
        my $sig = qq("$jfields{$type}");
        my $set = "JENV->Set${type}Field";

        print PFH <<EOF;
$pfields{$type}
$name($cname)
    $perl_class $cname

    CODE:
    RETVAL = $cname->$name;

    OUTPUT:
    RETVAL

EOF

        my $field_ix = $field_class_ix . "_\U$name";
        my $get_id = qq|JENV->GetFieldID(env, cls, "$jname", $sig)|;
        my $id_cache = "$field_class->ids[$field_ix]";

        my $id_lookup = $cache_field_ids ?
          $id_cache : $get_id;

        push @init_fields,
          "        $id_cache = ",
          "            $get_id;";

        push @macro,
          qq|    $set(env, obj, $id_lookup, s.$name);|;

        my $init = $jinit{$type} || '0';
        my $jtype = $jtype{$type} || lcfirst($type);
        my $platforms = supported_platforms($field->{plat});

        print JFH "    $jtype $jname = $init;\n\n";
        push @copy, "        copy.$jname = this.$jname;\n";

        #documentation
        print JFH "    /**\n";
        print JFH "     * Get the $desc.<p>\n";
        print JFH "     * Supported Platforms: $platforms.\n";
        print JFH "     * <p>\n";
        if (my $cmd = ($field->{cmd} || $cmds{$class})) {
            print JFH "     * System equivalent commands:<ul>\n";
            for my $p (sort keys %$cmd) {
                print JFH "     * <li> $p: <code>$cmd->{$p}</code><br>\n";
            }
            print JFH "     * </ul>\n";
        }
        print JFH "     * \@return $desc\n";
        print JFH "     */\n";

        print JFH "    public $jtype get\u$jname() { return $jname; }\n";
    }

    print JFH "\n    void copyTo($name copy) {\n", @copy, "    }\n";

    if (my $code = $extra_code{$name}) {
        print JFH $code;
    }

    push @init_fields, "    }";

    if ($cache_field_ids) {
        print HFH join(' \\' . "\n", @init_fields), "\n\n";
        print CFH "\n\n    $init_define(cls);\n\n" if $impl;
    }
    else {
        print HFH "#define $init_define(cls)\n";
    }

    print HFH join(' \\' . "\n", @macro), "\n\n";
    print CFH "\n\n    $define(cls, obj, s);" if $impl;

    print CFH "\n}\n" if $impl;
    print JFH "\n}\n";

    close JFH;
}

close CFH;
