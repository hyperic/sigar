/*
 * struct user is not binary compatible
 * between 4.3 and 5.x
 * this structure is taken from 5.1 sys/user.h
 * and defines from sys/user.h and sys/types.h
 * we compile this on v5.
 * XXX: will not compile 5.x  if we want to compile
 * on 5.x would need to create a struct user_v4
 * to maintain bincompt.
 */

#define V5_NISIG           3 /* v4 == 2 */

#define SIGMAX64 255
#define SIGMAX32 63
#ifdef __64BIT__
#define V5_SIGMAX SIGMAX64
#else
#define V5_SIGMAX SIGMAX32
#endif

#define NSIG64		(SIGMAX64+1)
#define NSIG32		(SIGMAX32+1)
#ifdef __64BIT__
#define V5_NSIG		NSIG64
#else
#define V5_NSIG		NSIG32
#endif

#define WLM_TAG_LENGTH 30
#define U_FD_LCK_CNT 16

typedef vmhandle_t      vmlpghandle_t;

typedef struct {
#ifdef _ALL_SOURCE
	unsigned int losigs, hisigs;
#else
	unsigned int __losigs, __hisigs;
#endif
} sigset32_t;

struct user_v5 {

	/* swappable process context */
	unsigned long long U_chk_paddr; /* address of checkpnt_pending var */
	struct saioq_head *U_saioq_head;/* anchor for async socket queue */
	uid_t		   U_sv_sgid;   /* set group identifier at exec time */
	int 		   U_num_pmsegs;/* number of PM segments */
	vmhandle_t 	   *U_pm_segs;	/* PM segments */
	vmhandle_t 	   U_pm_space;	/* space for first PM segment */
	vmhandle_t 	   U_text_vmh;	/* text seg vmhandle for memusage */
        unsigned long long U_cancel_func;/* cancelation user entry point */
        unsigned long long U_cancel_toc;/* cancelation user entry point */
	struct proc	*U_procp;	/* pointer to proc structure */
	Simple_lock	U_handy_lock;	/* self preservation lock */

#ifdef __64BIT_KERNEL
#ifdef _POWER
	/* U_segs32 must be pinned and must not cross a page boundary */
        void            *U_segs32_raddr; /* real addr of U_segs32 */
        vmhandle_t      U_segs32[NSEGS]; /* shadow of first 4Gb for resume() */
#endif /* _POWER */
#endif /* __64BIT_KERNEL */

	/* signal management */
	void		(*U_signal[NSIG32+V5_NISIG])(int);/* disposition of sigs */
	sigset32_t	U_sigmask[NSIG32+V5_NISIG];       /* sig's to be blocked */
#ifdef __64BIT_KERNEL
	uint		U_sigflags[V5_NSIG+V5_NISIG];	
#else
	ushort		U_sigflags[NSIG64+V5_NISIG];/* ushort-save space for now*/
#endif /* __64BIT_KERNEL */

	/* user-mode address space mapping */
#ifndef __64BIT_KERNEL
	adspace_t	U_adspace;	/* user-mode address space */
	struct segstate U_segst[NSEGS]; /* info on use of each segment */
#else
#ifdef _POWER
        void *    	U_uastrtp;      /* strt addr for V_USERACC */
	vmsize_t  	U_uasize;       /* size of the V_USERACC buf */
#endif /* _POWER */
	struct asalloc  U_snode;	/* segment node allocation */
	struct asalloc  U_unode;	/* uadspace node allocation */
        struct uadnode  U_adspace32[NADNODES]; /* usr adspace 32bit process */
        struct segnode  U_segnode[NSEGNODES]; /* segnode for 32bit processes */
#endif /* __64BIT_KERNEL */
	struct vmmlock 	U_adspace_lock;
	int		U_vmmflags;	/* vmm process state flags */

	/* auditing stuff */
	int		U_auditstatus;	/* auditing RESUME or SUSPEND */

	/* address map (mmap) */
	char		*U_map;

	/* current exec file information */
 	char		U_comm[MAXCOMLEN+1]; /* basename of exec file */

	int		U_ncargs;	     /* ARGMAX value during exec */

	/*
	 * Program model information, 64bit and 32bit, small and large data.
	 * The 'max' values are the maximums allowed by the model, which
	 * may be smaller than the current resource limits.
	 * These fields are filled in during exec.
	 *
	 * There is no U_stksize field.  The stack is allowed whatever it can
	 * get, subject to resource limits, model limits (possibly set from
	 * the a.out header during exec), and, in 32-bit mode, the PRIVSEG
	 * data break address.  The get_stack_size routine will return the
	 * current maximum effective size of the stack.
	 *
	 * Other fields:
	 *   U_breakhiwater: the high water mark for the break value
	 *   U_minbreak: the smallest permitted break value
	 *   U_min_mmap: If non-zero, smallest address allowed for mmap()
	 *		or shmat() at a user-specified address.
	 */
	unsigned long long U_tstart;	/* start of text	*/
	unsigned long long U_tsize;	/* text size (bytes)	*/
	unsigned long long U_datastart;	/* start of data	*/
	unsigned long long U_datasize;	/* Current data size (bytes)	*/

	/* DABR watchpoint information */
	unsigned long long U_wp_dabr;	/* DABR value for ptrace watchpoint */
	unsigned long long U_wp_value;	/* current value at watched address */

	/* System call table information */
	void		*U_svc_table;	/* svc table address  */
	int		U_max_svcnum;	/* max allowed svc number for process */

	/* CHANGES FROM HERE ON DO NOT AFFECT ASSEMBLER CODE
	   (see ml/POWER/32user.m4) */

	char		U_pad2[32];

	unsigned long long U_datamax;	/* Maximum data size (bytes)	*/
	unsigned long long U_minbreak;	/* minimum/initial break address */
	unsigned long long U_breakhiwater; /* Largest break address so far */
	unsigned long long U_min_mmap;	/* Minimum shmat/mmap address allowed */

	uint		U_brkseg;	/* segment number of U_breakhiwater */
	uint		U_stkseg;	/* Lowest segment number of stack */

	unsigned long long U_stkstart;	/* stack start (grows downwards) */
	unsigned long long U_stkmax;	/* stack max (bytes)	*/

	/*
	 * The following macros compute some commonly required values
	 * about the addressing model.
	 *
	 * U_BIGDATA	Tells you whether the 32-bit large data model
	 *		is in effect.
	 * U_PDATASIZE	Is the current data size for a 32-bit process
	 *		in the private segment (hence the P).
	 *		Note - this is always the PRIVSEG data size; with
	 *		the large data model, the general data size is
	 *		separate, as data begins in the BDATASEG.
	 * U_PDATABRK	Is the current data break address of a 32-bit process
	 *		in the private segment (see above).
	 * U_DATABRK	Is the general data break address in all cases.
	 * U_DSIZE	Is for compatibility only.  Use it to fill in fields
	 *		that previously were copies of U_dsize, which is now
	 *		obsolete.
	 */

	unsigned int	U_sdsize;	/* Size of data for privately loaded
					   modules, if the bigdata model is
					   being used. */
	short		U_lock; 	/* process/text locking flags */
	char		U_64bit;	/* 0 if 32-bit process, 0x1 if 64-bit */
	char		U_emul;		/* emulation type (UW7, etc.) */

	/* user identification and authorization */
	Simple_lock	U_cr_lock;	/* credentials lock */
        struct ucred    * volatile U_cred; /* user credentials (uid, gid, etc) */

	uinfo_t 	U_uinfo;	/* usrinfo() buffer */
	int		U_compatibility;/* compatibility/user mode bit masks */

	/* defines for u.u_compatibility bit mask */

	struct sem_undo	*U_semundo;	/* semaphore undo struct pointer */

	/* accounting and profiling data */
	time_t		U_start;
	time_t		U_ticks;
	struct profdata U_prof;
	short		U_acflag;	/* accounting flag */
	struct trusage64 U_ru;		/* this process resource usage value */
	struct trusage64 U_cru;		/* accumulated children's resources */
        /*
         * The kernel uses U_ru & U_cru to keep track of the time used by
         * the process and its children.
         * Even though the ru_utime & ru_stime fields within U_ru & U_cru are
         * struct timeval, their granularity within the kernel is nanoseconds,
         * not microseconds. This is the granularity returned by getprocs().
         * Other syscalls and library functions convert this to microseconds.
         */

	char		U_pthru;		/* pthread rusage tracking */

	/* virtualized resource usage values */
	struct trusage64 U_vru;
	struct trusage64 U_vcru;

	/* resource limits and counters */
	unsigned long	U_fsblimit;		/* fs limit in 512 byte-blks */

#if defined(_KERNSYS) || defined(__64BIT_KERNEL)
	/* rlimit32 structure is exposed to only 32/64 kernel 
	 * and 64bit kernel extensions 
	 */
	struct rlimit32	U_rlimit[RLIM_NLIMITS];	/* 32-bit resource limits */
#else
	struct rlimit	U_rlimit[RLIM_NLIMITS];	/* 32-bit resource limits */
#endif
	struct rlimit64 U_saved_rlimit[RLIM_NLIMITS]; /* saved 64-bit limits */
        /*
         * To maximize compatibility with old kernel code, a 32-bit
         * representation of each resource limit is maintained in U_rlimit.
         * Should the limit require a 64-bit representation, the U_rlimit
         * value is set to RLIM_INFINITY, with actual 64-bit limit being
         * stored in U_saved_rlimit.  These flags indicate what
         * the real situation is:
         *
         *   RLFLAG_SML => limit correctly represented in 32-bit U_rlimit
         *   RLFLAG_INF => limit is infinite
         *   RLFLAG_MAX => limit is in 64_bit U_saved_rlimit.rlim_max
         *   RLFLAG_CUR => limit is in 64_bit U_saved_rlimit.rlim_cur
         *
         * These flags are for use by the getrlimit/setrlimit routines only.
         * Kernel code desiring a 64-bit limit must go through kgetrlimit.
         */
        struct {
                uchar rlim_cur;                 /* how to determine rlim_cur */
                uchar rlim_max;                 /* how to determine rlim_max */
	} U_rlimit_flag[RLIM_NLIMITS];

	unsigned long long U_ioch; 		/* I/O character count */

	/* timers */
	Simple_lock	U_timer_lock;
	struct trb	*U_timer[NTIMERS];	/* per process timers */

	/* controlling tty info */
  	pid_t		*U_ttysid;	/* ptr to session leader id in tty */
	pid_t		*U_ttyp;	/* ptr to controlling tty pgrp field */
	dev_t		U_ttyd; 	/* controlling tty dev */
	long		U_ttympx;	/* mpx value for controlling tty */
	unsigned	*U_ttys;	/* pointer to t_state in tty struct */
	int32long64_t	U_ttyid;	/* tty id */
	int		(*U_ttyf)();	/* tty query function pointer */

	struct upfbuf	*U_message;	/* uprintf buffer pointer */
	struct trb *   	U_trb;		/* trb for user scheduler */
	struct pvthread	*U_chktv;	/* pointer to adv handler pvthread */

	/* file system state */
	vmid_t		U_pn_segno;	/* sid of chkpt/restart pathname seg*/
	Simple_lock	U_pnseg_lock;	/* lock of chkpt/restart pathname seg*/
	struct vnode	*U_cdir;	/* current directory of process */
	struct vnode	*U_rdir;	/* root directory of process */
	short		U_cmask;	/* mask for file creation */
	Simple_lock	U_fso_lock;	/* other file system fields lock */
	long		U_lockflag;	/* process has done file locks */
	long		U_fdevent;	/* fd lock event list */

        long long       U_irss;         /* accumulator for memory integral */
	struct pinu_block *U_pinu_block;/* list of control structs for pinu */
	tid_t		U_ulocks;	/* event list for user locks */
	int32long64_t	U_rmmap;	/* anchor of rmmap list	*/

	unsigned long long U_loader[84]; /* loader area */
	Simple_lock     U_aio_susp_lock; /* aio lock used in aio_suspend */
	unsigned int    U_fdfree[2];    /* file descriptor management */

	unsigned int    U_cannot_chpt;  /* process unable to checkpoint */
	unsigned int	U_maxofile;	/* maximum u_ofile index in use */
	unsigned int	U_freefile;	/* first available u_ofile index */

	/* WLM data */
	dev64_t		U_dev;		/* device of exec'ed file */
	ino64_t		U_ino;		/* inode */
	uint_t		U_gen;		/* generation number */

	char		U_tag[WLM_TAG_LENGTH+1]; /* WLM tag: user settable string */

	/*  structures added for VMM checkpoint/restart support */
        struct ckpt_ipcid *U_ckptshm;   /* ptr to shmid blocks for checkpoint   */
        struct ckpt_ipcid *U_ckptsem;   /* ptr to semid blocks for checkpoint   */
        struct ckpt_ipcid *U_ckptmsg;   /* ptr to msgid blocks for checkpoint   */
        struct _mmapent   *U_mapent;    /* ptr to mmap entry structure used for restart */
	ut_error_t	   U_ckpterr;   /* u_error saved during ckpt processing */

#ifdef __64BIT_KERNEL
	char		cacheline_pad[128]; /* keep 1st cacheline alligned */
#else
	vmlpghandle_t   U_lpgsegs32[NSEGS]; /* lgpg handles for 32bit processes */
	char		cacheline_pad[64]; /* keep 1st cacheline alligned */
#endif /* __64BIT_KERNEL */
	struct {		  	    /* file descriptor lock cacheline */
		Simple_lock	u_fd_slock;
		char		cache_pad[128 - sizeof(Simple_lock)];
	} U_fd_slcks[U_FD_LCK_CNT]; 

	/* from here on the data is pageable. */

	struct ufd	U_ufd[OPEN_MAX];/* User's file descriptor table */
};
