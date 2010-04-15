INCLUDES = @INCLUDES@

SIGAR_OS_SRCS = solaris_sigar.c get_mib2.c kstats.c procfs.c

SIGAR_OS_HDRS = sigar_os.h get_mib2.h

if OS_SOLARIS
noinst_LTLIBRARIES = libsigar_os.la

libsigar_os_la_SOURCES = ${SIGAR_OS_SRCS}

noinst_HEADERS = ${SIGAR_OS_HDRS}
else
EXTRA_DIST=\
	${SIGAR_OS_SRCS} \
	${SIGAR_OS_HDRS}
endif

