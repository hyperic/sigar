INCLUDES = @INCLUDES@

SIGAR_OS_SRCS = peb.c win32_sigar.c wmi.cpp

SIGAR_OS_HDRS = sigar_os.h sigar_pdh.h

if OS_WIN32
noinst_LTLIBRARIES = libsigar_os.la

libsigar_os_la_SOURCES = ${SIGAR_OS_SRCS}

noinst_HEADERS = ${SIGAR_OS_HDRS}
else
EXTRA_DIST=\
	${SIGAR_OS_SRCS} \
	${SIGAR_OS_HDRS}
endif

