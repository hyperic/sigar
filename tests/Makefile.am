TESTS = \
	t_sigar_cpu \
	t_sigar_proc \
	t_sigar_swap \
	t_sigar_mem \
	t_sigar_sysinfo \
	t_sigar_version \
	t_sigar_loadavg \
	t_sigar_uptime \
	t_sigar_reslimit \
	t_sigar_fs \
	t_sigar_netif \
	t_sigar_netconn \
	t_sigar_pid

if USE_VALGRIND
TESTS_ENVIRONMENT = \
	VALGRIND=${VALGRIND} \
	${srcdir}/valgrind-leak-check
endif

check_PROGRAMS = \
	$(TESTS)

t_sigar_mem_SOURCES = t_sigar_mem.c
t_sigar_mem_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_pid_SOURCES = t_sigar_pid.c
t_sigar_pid_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_swap_SOURCES = t_sigar_swap.c
t_sigar_swap_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_cpu_SOURCES = t_sigar_cpu.c
t_sigar_cpu_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_proc_SOURCES = t_sigar_proc.c
t_sigar_proc_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_sysinfo_SOURCES = t_sigar_sysinfo.c
t_sigar_sysinfo_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_uptime_SOURCES = t_sigar_uptime.c
t_sigar_uptime_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_loadavg_SOURCES = t_sigar_loadavg.c
t_sigar_loadavg_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_version_SOURCES = t_sigar_version.c
t_sigar_version_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_fs_SOURCES = t_sigar_fs.c
t_sigar_fs_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_reslimit_SOURCES = t_sigar_reslimit.c
t_sigar_reslimit_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_netif_SOURCES = t_sigar_netif.c
t_sigar_netif_LDADD = $(top_builddir)/src/libsigar.la

t_sigar_netconn_SOURCES = t_sigar_netconn.c
t_sigar_netconn_LDADD = $(top_builddir)/src/libsigar.la

noinst_HEADERS=\
	 sigar_tests.h

EXTRA_DIST=\
	 valgrind-leak-check \
	CMakeLists.txt
