/* IBM_PROLOG_BEGIN_TAG                                                   */
/* This is an automatically generated prolog.                             */
/*                                                                        */
/* bos52B src/bos/usr/ccs/lib/libperfstat/libperfstat.h 1.4.1.6           */
/*                                                                        */
/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* Restricted Materials of IBM                                            */
/*                                                                        */
/* (C) COPYRIGHT International Business Machines Corp. 2000,2003          */
/* All Rights Reserved                                                    */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or            */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.      */
/*                                                                        */
/* IBM_PROLOG_END_TAG                                                     */
/*
 * @(#)91       1.4.1.6     src/bos/usr/ccs/lib/libperfstat/libperfstat.h, libperfstat, bos52B, b2003_06F6 2/6/03 06:43:34
 * LEVEL 1,  5 Years Bull Confidential Information
 *
 */
#ifndef LIBPERFSTAT_H
#define LIBPERFSTAT_H

/* This file describes the structures and constants used by the libperfstat API */

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#define IDENTIFIER_LENGTH 64    /* length of strings included in the structures */

#define FIRST_CPU  ""           /* pseudo-name for the first logical cpu */
#define FIRST_DISK  ""          /* pseudo-name for the first disk */
#define FIRST_DISKPATH ""       /* pseudo-name for the first disk path */
#define FIRST_DISKADAPTER  ""   /* pseudo-name for the first disk adapter */
#define FIRST_NETINTERFACE  ""  /* pseudo-name for the first network interface */
#define FIRST_PAGINGSPACE  ""   /* pseudo-name for the first paging space */
#define FIRST_PROTOCOL  ""      /* pseudo-name for the first protocol */
#define FIRST_NETBUFFER  ""     /* pseudo-name for the first network buffer size */

#define DEFAULT_DEF  "not available"

typedef struct { /* structure element identifier */
	char name[IDENTIFIER_LENGTH]; /* name of the identifier */
} perfstat_id_t;

typedef struct { /* cpu information */
	char name[IDENTIFIER_LENGTH]; /* logical processor name (cpu0, cpu1, ..) */
	u_longlong_t user;     /* raw number of clock ticks spent in user mode */
	u_longlong_t sys;      /* raw number of clock ticks spent in system mode */
	u_longlong_t idle;     /* raw number of clock ticks spent idle */
	u_longlong_t wait;     /* raw number of clock ticks spent waiting for I/O */
	u_longlong_t pswitch;  /* number of context switches (changes of currently running process) */
	u_longlong_t syscall;  /* number of system calls executed */
	u_longlong_t sysread;  /* number of read system calls executed */
	u_longlong_t syswrite; /* number of write system calls executed */
	u_longlong_t sysfork;  /* number of fork system call executed */
	u_longlong_t sysexec;  /* number of exec system call executed */
	u_longlong_t readch;   /* number of characters tranferred with read system call */
	u_longlong_t writech;  /* number of characters tranferred with write system call */
	u_longlong_t bread;    /* number of block reads */
	u_longlong_t bwrite;   /* number of block writes */
	u_longlong_t lread;    /* number of logical read requests */
	u_longlong_t lwrite;   /* number of logical write requests */
	u_longlong_t phread;   /* number of physical reads (reads on raw device) */
	u_longlong_t phwrite;  /* number of physical writes (writes on raw device) */
	u_longlong_t iget;     /* number of inode lookups */
	u_longlong_t namei;    /* number of vnode lookup from a path name */
	u_longlong_t dirblk;   /* number of 512-byte block reads by the directory search routine to locate an entry for a file */
	u_longlong_t msg;      /* number of IPC message operations */
	u_longlong_t sema;     /* number of IPC semaphore operations */
} perfstat_cpu_t;

typedef struct { /* global cpu information */
	int ncpus;                /* number of active logical processors */
	int ncpus_cfg;	           /* number of configured processors */
	char description[IDENTIFIER_LENGTH]; /* processor description (type/official name) */
	u_longlong_t processorHZ; /* processor speed in Hz */
	u_longlong_t user;        /*  raw total number of clock ticks spent in user mode */
	u_longlong_t sys;         /* raw total number of clock ticks spent in system mode */
	u_longlong_t idle;        /* raw total number of clock ticks spent idle */
	u_longlong_t wait;        /* raw total number of clock ticks spent waiting for I/O */
	u_longlong_t pswitch;     /* number of process switches (change in currently running process) */
	u_longlong_t syscall;     /* number of system calls executed */
	u_longlong_t sysread;     /* number of read system calls executed */
	u_longlong_t syswrite;    /* number of write system calls executed */
	u_longlong_t sysfork;     /* number of forks system calls executed */
	u_longlong_t sysexec;     /* number of execs system calls executed */
	u_longlong_t readch;      /* number of characters tranferred with read system call */
	u_longlong_t writech;     /* number of characters tranferred with write system call */
	u_longlong_t devintrs;    /* number of device interrupts */
	u_longlong_t softintrs;   /* number of software interrupts */
	time_t lbolt;	           /* number of ticks since last reboot */
	u_longlong_t loadavg[3];  /* (1<<SBITS) times the average number of runnables processes during the last 1, 5 and 15 minutes.    */
	                          /* To calculate the load average, divide the numbers by (1<<SBITS). SBITS is defined in <sys/proc.h>. */
	u_longlong_t runque;      /* length of the run queue (processes ready) */
	u_longlong_t swpque;      /* ength of the swap queue (processes waiting to be paged in) */
	u_longlong_t bread;       /* number of blocks read */
	u_longlong_t bwrite;      /* number of blocks written */
	u_longlong_t lread;       /* number of logical read requests */
	u_longlong_t lwrite;      /* number of logical write requests */
	u_longlong_t phread;      /* number of physical reads (reads on raw devices) */
	u_longlong_t phwrite;     /* number of physical writes (writes on raw devices) */
	u_longlong_t runocc;      /* updated whenever runque is updated, i.e. the runqueue is occupied.
	                           * This can be used to compute the simple average of ready processes  */
	u_longlong_t swpocc;      /* updated whenever swpque is updated. i.e. the swpqueue is occupied.
	                           * This can be used to compute the simple average processes waiting to be paged in */
	u_longlong_t iget;        /* number of inode lookups */
	u_longlong_t namei;       /* number of vnode lookup from a path name */
	u_longlong_t dirblk;      /* number of 512-byte block reads by the directory search routine to locate an entry for a file */
	u_longlong_t msg;         /* number of IPC message operations */
	u_longlong_t sema;        /* number of IPC semaphore operations */
	u_longlong_t rcvint;      /* number of tty receive interrupts */
	u_longlong_t xmtint;      /* number of tyy transmit interrupts */
	u_longlong_t mdmint;      /* number of modem interrupts */
	u_longlong_t tty_rawinch; /* number of raw input characters  */
	u_longlong_t tty_caninch; /* number of canonical input characters (always zero) */
	u_longlong_t tty_rawoutch;/* number of raw output characters */
	u_longlong_t ksched;      /* number of kernel processes created */
	u_longlong_t koverf;      /* kernel process creation attempts where:
	                           * -the user has forked to their maximum limit
	                           * -the configuration limit of processes has been reached */
	u_longlong_t kexit;       /* number of kernel processes that became zombies */
	u_longlong_t rbread;      /* number of remote read requests */
	u_longlong_t rcread;      /* number of cached remote reads */
	u_longlong_t rbwrt;       /* number of remote writes */
	u_longlong_t rcwrt;       /* number of cached remote writes */
	u_longlong_t traps;       /* number of traps */
	int ncpus_high;           /* index of highest processor online */
} perfstat_cpu_total_t;

typedef struct { /* disk information */
	char name[IDENTIFIER_LENGTH];        /* name of the disk */
	char description[IDENTIFIER_LENGTH]; /* disk description (from ODM) */
	char vgname[IDENTIFIER_LENGTH];      /* volume group name (from ODM) */
	u_longlong_t size;                   /* size of the disk (in MB) */
	u_longlong_t free;                   /* free portion of the disk (in MB) */
	u_longlong_t bsize;                  /* disk block size (in bytes) */
	u_longlong_t xrate;                  /* kbytes/sec xfer rate capability */
	u_longlong_t xfers;                  /* number of transfers to/from disk */
	u_longlong_t wblks;                  /* number of blocks written to disk */
	u_longlong_t rblks;                  /* number of blocks read from disk */
	u_longlong_t qdepth;                 /* queue depth */
	u_longlong_t time;                   /* amount of time disk is active */
	char adapter[IDENTIFIER_LENGTH];     /* disk adapter name (from ODM) */
	uint paths_count;                    /* number of paths defined to the disk */
} perfstat_disk_t;

typedef struct { /* global disk information */
	int number;          /* total number of disks */
	u_longlong_t size;   /* total size of all disks (in MB) */
	u_longlong_t free;   /* free portion of all disks (in MB) */
	u_longlong_t xrate;  /* total kbytes/sec xfer rate capability */
	u_longlong_t xfers;  /* total number of transfers to/from disk */
	u_longlong_t wblks;  /* 512 bytes blocks written to all disks */
	u_longlong_t rblks;  /* 512 bytes blocks read from all disks */
	u_longlong_t time;   /* amount of time disks are active */
} perfstat_disk_total_t;

typedef struct { /* Disk adapter information */
	char name[IDENTIFIER_LENGTH];        /* name of the adapter (from ODM) */
	char description[IDENTIFIER_LENGTH]; /* adapter description (from ODM) */
	int number;         /* number of disks connected to adapter */
	u_longlong_t size;  /* total size of all disks (in MB)  */
	u_longlong_t free;  /* free portion of all disks (in MB)  */
	u_longlong_t xrate; /* total kbytes/sec xfer rate capability */
	u_longlong_t xfers; /* total number of transfers to/from disk */
	u_longlong_t rblks; /* 512 bytes blocks written via adapter */
	u_longlong_t wblks; /* 512 bytes blocks read via adapter  */
	u_longlong_t time;  /* amount of time disks are active */
} perfstat_diskadapter_t;

typedef struct { /* mpio information */
	char name[IDENTIFIER_LENGTH];        /* name of the path */
	u_longlong_t xrate; /* total kbytes/sec xfer rate capability */
	u_longlong_t xfers; /* total number of transfers via the path */
	u_longlong_t rblks; /* 512 bytes blocks written via the path */
	u_longlong_t wblks; /* 512 bytes blocks read via the path  */
	u_longlong_t time;  /* amount of time disks are active */
	char adapter[IDENTIFIER_LENGTH]; /* disk adapter name (from ODM) */
} perfstat_diskpath_t;

typedef struct { /* Virtual memory utilization */
	u_longlong_t virt_total;    /* total virtual memory (in 4KB pages) */
	u_longlong_t real_total;    /* total real memory (in 4KB pages) */
	u_longlong_t real_free;     /* free real memory (in 4KB pages) */
	u_longlong_t real_pinned;   /* real memory which is pinned (in 4KB pages) */
	u_longlong_t real_inuse;    /* real memory which is in use (in 4KB pages) */
	u_longlong_t pgbad;         /* number of bad pages */
	u_longlong_t pgexct;        /* number of page faults */
	u_longlong_t pgins;         /* number of pages paged in */
	u_longlong_t pgouts;        /* number of pages paged out */
	u_longlong_t pgspins;       /* number of page ins from paging space */
	u_longlong_t pgspouts;      /* number of page outs from paging space */
	u_longlong_t scans;         /* number of page scans by clock */
	u_longlong_t cycles;        /* number of page replacement cycles */
	u_longlong_t pgsteals;      /* number of page steals */
	u_longlong_t numperm;       /* number of frames used for files (in 4KB pages) */
	u_longlong_t pgsp_total;    /* total paging space (in 4KB pages) */
	u_longlong_t pgsp_free;     /* free paging space (in 4KB pages) */
	u_longlong_t pgsp_rsvd;     /* reserved paging space (in 4KB pages) */
	u_longlong_t real_system;   /* real memory used by system segments (in 4KB pages). This is the sum of all the used pages in segment marked for system usage.
	                             * Since segment classifications are not always guaranteed to be accurate, this number is only an approximation. */
	u_longlong_t real_user;     /* real memory used by non-system segments (in 4KB pages). This is the sum of all pages used in segments not marked for system usage.
	                             * Since segment classifications are not always guaranteed to be accurate, this number is only an approximation. */
	u_longlong_t real_process;  /* real memory used by process segments (in 4KB pages). This is real_total-real_free-numperm-real_system. Since real_system is an
	                             * approximation, this number is too. */

} perfstat_memory_total_t;

typedef struct { /* Description of the network interface */
	char name[IDENTIFIER_LENGTH];   /* name of the interface */
	char description[IDENTIFIER_LENGTH]; /* interface description (from ODM, similar to lscfg output) */
	uchar type;               /* ethernet, tokenring, etc. interpretation can be done using /usr/include/net/if_types.h */
	u_longlong_t mtu;         /* network frame size */
	u_longlong_t ipackets;    /* number of packets received on interface */
	u_longlong_t ibytes;      /* number of bytes received on interface */
	u_longlong_t ierrors;     /* number of input errors on interface */
	u_longlong_t opackets;    /* number of packets sent on interface */
	u_longlong_t obytes;      /* number of bytes sent on interface */
	u_longlong_t oerrors;     /* number of output errors on interface */
	u_longlong_t collisions;  /* number of collisions on csma interface */
	u_longlong_t bitrate;     /* adapter rating in bit per second */
} perfstat_netinterface_t;

typedef struct { /* Description of the network interfaces */
	int number;               /* number of network interfaces  */
	u_longlong_t ipackets;    /* number of packets received on interface */
	u_longlong_t ibytes;      /* number of bytes received on interface */
	u_longlong_t ierrors;     /* number of input errors on interface */
	u_longlong_t opackets;    /* number of packets sent on interface */
	u_longlong_t obytes;      /* number of bytes sent on interface */
	u_longlong_t oerrors;     /* number of output errors on interface */
	u_longlong_t collisions;  /* number of collisions on csma interface */
} perfstat_netinterface_total_t;

enum {
	LV_PAGING=1,
	NFS_PAGING
};

typedef struct { /* Paging space data for a specific logical volume */
	char name[IDENTIFIER_LENGTH];    /* Paging space name */
	char type; /* type of paging device (LV_PAGING or NFS_PAGING) *
		    * Possible values are:                            *
		    *     LV_PAGING      logical volume               *
		    *     NFS_PAGING     NFS file                     */
	union{
	       struct{
		      char hostname[IDENTIFIER_LENGTH]; /* host name of paging server */
		      char filename[IDENTIFIER_LENGTH]; /* swap file name on server  */
	       } nfs_paging;
	       struct{
		      char vgname[IDENTIFIER_LENGTH];/*  volume group name  */
	       } lv_paging;
	} id;
	longlong_t lp_size;    /* size in number of logical partitions  */
	longlong_t mb_size;    /* size in megabytes  */
	longlong_t mb_used;    /* portion used in megabytes  */
	longlong_t io_pending; /* number of pending I/O */
	char active;           /* indicates if active (1 if so, 0 if not) */
	char automatic;        /* indicates if automatic (1 if so, 0 if not) */
} perfstat_pagingspace_t;

typedef struct {  /* network buffers  */
	char name[IDENTIFIER_LENGTH]; /* size in ascii, always power of 2 (ex: "32", "64", "128") */
	u_longlong_t inuse;           /* number of buffer currently allocated */
	u_longlong_t calls;           /* number of buffer allocations since last reset */
	u_longlong_t delayed;         /* number of delayed allocations */
	u_longlong_t free;            /* number of free calls */
	u_longlong_t failed;          /* number of failed allocations */
	u_longlong_t highwatermark;   /* high threshold for number of buffer allocated */
	u_longlong_t freed;           /* number of buffers freed */
} perfstat_netbuffer_t;

typedef struct { /* utilization of protocols */
	char name[IDENTIFIER_LENGTH]; /* ip, ipv6, icmp, icmpv6, udp, tcp, rpc, nfs, nfsv2, nfsv3*/
	union{
		struct{
			u_longlong_t ipackets;       /* number of input packets */
			u_longlong_t ierrors;        /* number of input errors */
			u_longlong_t iqueueoverflow; /* number of input queue overflows */
			u_longlong_t opackets;       /* number of output packets */
			u_longlong_t oerrors;        /* number of output errors */
		} ip;
		struct{
			u_longlong_t ipackets;       /* number of input packets */	   
			u_longlong_t ierrors;        /* number of input errors */	   
			u_longlong_t iqueueoverflow; /* number of input queue overflows */
			u_longlong_t opackets;       /* number of output packets */
			u_longlong_t oerrors;        /* number of output errors */
		} ipv6;
		struct{
			u_longlong_t received; /* number of packets received */
			u_longlong_t sent;     /* number of packets sent */
			u_longlong_t errors;   /* number of errors */
		} icmp;
		struct{
			u_longlong_t received; /* number of packets received */
			u_longlong_t sent;     /* number of packets sent */
			u_longlong_t errors;   /* number of errors */
		} icmpv6;
		struct{
			u_longlong_t ipackets;  /* number of input packets */
			u_longlong_t ierrors;   /* number of input errors */
			u_longlong_t opackets;  /* number of output packets */
			u_longlong_t no_socket; /* number of packets dropped due to no socket */
		} udp;
		struct{
			u_longlong_t ipackets;    /* number of input packets */
			u_longlong_t ierrors;     /* number of input errors */
			u_longlong_t opackets;    /* number of output packets */
			u_longlong_t initiated;   /* number of connections initiated */
			u_longlong_t accepted;    /* number of connections accepted */
			u_longlong_t established; /* number of connections established */
			u_longlong_t dropped;     /* number of connections dropped */
		} tcp;
		struct{
			struct{
				struct{
					u_longlong_t calls;     /* total NFS client RPC connection-oriented calls */
					u_longlong_t badcalls;  /* rejected NFS client RPC calls */
					u_longlong_t badxids;   /* bad NFS client RPC call responses */
					u_longlong_t timeouts;  /* timed out NFS client RPC calls with no reply */
					u_longlong_t newcreds;  /* total NFS client RPC authentication refreshes */
					u_longlong_t badverfs;  /* total NFS client RPC bad verifier in response */
					u_longlong_t timers;    /* NFS client RPC timout greater than timeout value */
					u_longlong_t nomem;     /* NFS client RPC calls memory allocation failure */
					u_longlong_t cantconn;  /* failed NFS client RPC calls */
					u_longlong_t interrupts;/* NFS client RPC calls fail due to interrupt */
				} stream; /* connection oriented rpc client */
				struct{
					u_longlong_t calls;    /* total NFS client RPC connectionless calls */
					u_longlong_t badcalls; /* rejected NFS client RPC calls */
					u_longlong_t retrans;  /* retransmitted NFS client RPC calls */
					u_longlong_t badxids;  /* bad NFS client RPC call responses */
					u_longlong_t timeouts; /* timed out NFS client RPC calls with no reply */
					u_longlong_t newcreds; /* total NFS client RPC authentication refreshes */
					u_longlong_t badverfs; /* total NFS client RPC bad verifier in response */
					u_longlong_t timers;   /* NFS client RPC timout greater than timeout value */
					u_longlong_t nomem;    /* NFS client RPC calls memory allocation failure */
					u_longlong_t cantsend; /* NFS client RPC calls not sent */
				} dgram; /* connection less rpc client */
			} client; /* rpc client */
			struct{
				struct{
					u_longlong_t calls;    /* total NFS server RPC connection-oriented requests */
					u_longlong_t badcalls; /* rejected NFS server RPC requests */
					u_longlong_t nullrecv; /* NFS server RPC calls failed due to unavailable packet */
					u_longlong_t badlen;   /* NFS server RPC requests failed due to bad length */
					u_longlong_t xdrcall;  /* NFS server RPC requests failed due to bad header */
					u_longlong_t dupchecks;/* NFS server RPC calls found in request cache */
					u_longlong_t dupreqs;  /* total NFS server RPC call duplicates */
				} stream; /* connection oriented rpc server */
				struct{
					u_longlong_t calls;    /* total NFS server RPC connectionless requests */
					u_longlong_t badcalls; /* rejected NFS server RPC requests */
					u_longlong_t nullrecv; /* NFS server RPC calls failed due to unavailable packet */
					u_longlong_t badlen;   /* NFS server RPC requests failed due to bad length */
					u_longlong_t xdrcall;  /* NFS server RPC requests failed due to bad header */
					u_longlong_t dupchecks;/* NFS server RPC calls found in request cache */
					u_longlong_t dupreqs;  /* total NFS server RPC call duplicates */
				} dgram; /* connection less rpc server */
			} server; /* rpc server*/
		} rpc;
	      struct{
		      struct{
			      u_longlong_t calls;    /* total NFS client requests */
			      u_longlong_t badcalls; /* total NFS client failed calls */
			      u_longlong_t clgets;   /* total number of client nfs clgets */
			      u_longlong_t cltoomany;/* total number of client nfs cltoomany */

		      } client; /* nfs client */
		      struct{
			      u_longlong_t calls;     /* total NFS server requests */
			      u_longlong_t badcalls;  /* total NFS server failed calls */
			      u_longlong_t public_v2; /* total number of nfs version 2 server calls */
			      u_longlong_t public_v3; /* total number of nfs version 3 server calls */
		      } server; /* nfs server */
	      } nfs;
	      struct{
		      struct{
			      u_longlong_t calls;     /* NFS V2 client requests  */
			      u_longlong_t null;      /* NFS V2 client null requests */
			      u_longlong_t getattr;   /* NFS V2 client getattr requests */
			      u_longlong_t setattr;   /* NFS V2 client setattr requests */
			      u_longlong_t root;      /* NFS V2 client root requests */
			      u_longlong_t lookup;    /* NFS V2 client file name lookup requests */
			      u_longlong_t readlink;  /* NFS V2 client readlink requests */
			      u_longlong_t read;      /* NFS V2 client read requests */
			      u_longlong_t writecache;/* NFS V2 client write cache requests */
			      u_longlong_t write;     /* NFS V2 client write requests */
			      u_longlong_t create;    /* NFS V2 client file creation requests */
			      u_longlong_t remove;    /* NFS V2 client file removal requests */
			      u_longlong_t rename;    /* NFS V2 client file rename requests */
			      u_longlong_t link;      /* NFS V2 client link creation requests */
			      u_longlong_t symlink;   /* NFS V2 client symbolic link creation requests */
			      u_longlong_t mkdir;     /* NFS V2 client directory creation requests */
			      u_longlong_t rmdir;     /* NFS V2 client directory removal requests */
			      u_longlong_t readdir;   /* NFS V2 client read-directory requests */
			      u_longlong_t statfs;    /* NFS V2 client file stat requests */
		      } client; /* nfs2 client */
		      struct{
			      u_longlong_t calls;     /* NFS V2 server requests */
			      u_longlong_t null;      /* NFS V2 server null requests */
			      u_longlong_t getattr;   /* NFS V2 server getattr requests */
			      u_longlong_t setattr;   /* NFS V2 server setattr requests */
			      u_longlong_t root;      /* NFS V2 server root requests */
			      u_longlong_t lookup;    /* NFS V2 server file name lookup requests */
			      u_longlong_t readlink;  /* NFS V2 server readlink requests */
			      u_longlong_t read;      /* NFS V2 server read requests */
			      u_longlong_t writecache;/* NFS V2 server cache requests */
			      u_longlong_t write;     /* NFS V2 server write requests */
			      u_longlong_t create;    /* NFS V2 server file creation requests */
			      u_longlong_t remove;    /* NFS V2 server file removal requests */
			      u_longlong_t rename;    /* NFS V2 server file rename requests */
			      u_longlong_t link;      /* NFS V2 server link creation requests */
			      u_longlong_t symlink;   /* NFS V2 server symbolic link creation requests */
			      u_longlong_t mkdir;     /* NFS V2 server directory creation requests */
			      u_longlong_t rmdir;     /* NFS V2 server directory removal requests */
			      u_longlong_t readdir;   /* NFS V2 server read-directory requests */
			      u_longlong_t statfs;    /* NFS V2 server file stat requests */
		      } server; /* nfsv2 server */
	      } nfsv2;
	      struct{
		      struct{
			      u_longlong_t calls;       /* NFS V3 client calls */
			      u_longlong_t null;        /* NFS V3 client null requests */
			      u_longlong_t getattr;     /* NFS V3 client getattr requests */
			      u_longlong_t setattr;     /* NFS V3 client setattr requests */
			      u_longlong_t lookup;      /* NFS V3 client file name lookup requests */
			      u_longlong_t access;      /* NFS V3 client access requests */
			      u_longlong_t readlink;    /* NFS V3 client readlink requests */
			      u_longlong_t read;        /* NFS V3 client read requests */
			      u_longlong_t write;       /* NFS V3 client write requests */
			      u_longlong_t create;      /* NFS V3 client file creation requests */
			      u_longlong_t mkdir;       /* NFS V3 client directory creation requests */
			      u_longlong_t symlink;     /* NFS V3 client symbolic link creation requests */
			      u_longlong_t mknod;       /* NFS V3 client mknod creation requests */
			      u_longlong_t remove;      /* NFS V3 client file removal requests */
			      u_longlong_t rmdir;       /* NFS V3 client directory removal requests */
			      u_longlong_t rename;      /* NFS V3 client file rename requests */
			      u_longlong_t link;        /* NFS V3 client link creation requests */
			      u_longlong_t readdir;     /* NFS V3 client read-directory requests */
			      u_longlong_t readdirplus; /* NFS V3 client read-directory plus requests */
			      u_longlong_t fsstat;      /* NFS V3 client file stat requests */
			      u_longlong_t fsinfo;      /* NFS V3 client file info requests */
			      u_longlong_t pathconf;    /* NFS V3 client path configure requests */
			      u_longlong_t commit;      /* NFS V3 client commit requests */
		      } client; /* nfsv3 client */
		      struct{
			      u_longlong_t calls;       /* NFS V3 server requests */
			      u_longlong_t null;        /* NFS V3 server null requests */
			      u_longlong_t getattr;     /* NFS V3 server getattr requests */
			      u_longlong_t setattr;     /* NFS V3 server setattr requests */
			      u_longlong_t lookup;      /* NFS V3 server file name lookup requests */
			      u_longlong_t access;      /* NFS V3 server file access requests */
			      u_longlong_t readlink;    /* NFS V3 server readlink requests */
			      u_longlong_t read;        /* NFS V3 server read requests */
			      u_longlong_t write;       /* NFS V3 server write requests */
			      u_longlong_t create;      /* NFS V3 server file creation requests */
			      u_longlong_t mkdir;       /* NFS V3 server director6 creation requests */
			      u_longlong_t symlink;     /* NFS V3 server symbolic link creation requests */
			      u_longlong_t mknod;       /* NFS V3 server mknode creation requests */
			      u_longlong_t remove;      /* NFS V3 server file removal requests */
			      u_longlong_t rmdir;       /* NFS V3 server directory removal requests */
			      u_longlong_t rename;      /* NFS V3 server file rename requests */
			      u_longlong_t link;        /* NFS V3 server link creation requests */
			      u_longlong_t readdir;     /* NFS V3 server read-directory requests */
			      u_longlong_t readdirplus; /* NFS V3 server read-directory plus requests */
			      u_longlong_t fsstat;      /* NFS V3 server file stat requests */
			      u_longlong_t fsinfo;      /* NFS V3 server file info requests */
			      u_longlong_t pathconf;	  /* NFS V3 server path configure requests */
			      u_longlong_t commit;      /* NFS V3 server commit requests */
		      } server; /* nfsv3 server */
		} nfsv3;
	} u;
} perfstat_protocol_t;

extern int perfstat_cpu_total(perfstat_id_t *name,
                              perfstat_cpu_total_t* userbuff,
                              int sizeof_userbuff,
                              int desired_number);
extern int perfstat_cpu(perfstat_id_t *name,
                        perfstat_cpu_t* userbuff,
                        int sizeof_userbuff,
                        int desired_number);
extern int perfstat_disk_total(perfstat_id_t *name,
                               perfstat_disk_total_t* userbuff,
                               int sizeof_userbuff,
                               int desired_number);
extern int perfstat_disk(perfstat_id_t *name,
                         perfstat_disk_t* userbuff,
                         int sizeof_userbuff,
                         int desired_number);
extern int perfstat_diskadapter(perfstat_id_t *name,
                                perfstat_diskadapter_t* userbuff,      
                                int sizeof_userbuff,		       
                                int desired_number);		       
extern int perfstat_diskpath(perfstat_id_t *name,
                             perfstat_diskpath_t* userbuff,      
                             int sizeof_userbuff,		       
                             int desired_number);		       
extern int perfstat_memory_total(perfstat_id_t *name,
                                 perfstat_memory_total_t* userbuff,
                                 int sizeof_userbuff,
                                 int desired_number);
extern int perfstat_netinterface_total(perfstat_id_t *name,
                                       perfstat_netinterface_total_t* userbuff,
                                       int sizeof_userbuff,
                                       int desired_number);
extern int perfstat_netinterface(perfstat_id_t *name,
                                 perfstat_netinterface_t* userbuff,
                                 int sizeof_userbuff,
                                 int desired_number);

extern int perfstat_pagingspace(perfstat_id_t *name,
                                perfstat_pagingspace_t* userbuff,	   
                                int sizeof_userbuff,			   
                                int desired_number);			   

extern int perfstat_netbuffer(perfstat_id_t *name,
                              perfstat_netbuffer_t* userbuff,
                              int sizeof_userbuff,
                              int desired_number);

extern int perfstat_protocol(perfstat_id_t *name,
                             perfstat_protocol_t* userbuff,
                             int sizeof_userbuff,
                             int desired_number);

extern void perfstat_reset(void);

#ifdef __cplusplus
}
#endif

#endif /*undef LIBPERFSTAT_H*/

