/*
 * struct utmp is not binary compatible
 * between 4.3 and 5.x
 * this structure is taken from 5.1 utmp.h
 * we compile this on v4.3.
 * XXX: will not compile 5.x  if we want to compile
 * on 5.x would need to create a struct user_v4
 * to maintain bincompt.
 */

struct utmp_v5 {
	char ut_user[256] ;		/* User login name */
	char ut_id[14] ;		/* /etc/inittab id */
	char ut_line[64] ;		/* device name (console, lnxx) */
	pid_t ut_pid ;			/* process id */
	short ut_type ; 		/* type of entry */
#if !defined(__64BIT__) && !defined(__ia64)
        int __time_t_space;             /* for 32vs64-bit time_t PPC */
#endif
	time_t ut_time ;		/* time entry was made */
#if !defined(__64BIT__) && defined(__ia64)
        int __time_t_space;             /* for 32vs64-bit time_t IA64 */
#endif
	struct exit_status_v5
	  {
	    short e_termination ;	/* Process termination status */
	    short e_exit ;		/* Process exit status */
	  }
	ut_exit ;			/* The exit status of a process
					 * marked as DEAD_PROCESS.
					 */
	char ut_host[256] ;		/* host name */
	int __dbl_word_pad;		/* for double word alignment */
	int __reservedA[2];
	int __reservedV[6];
};
