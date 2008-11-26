SUBDIRS = os

INCLUDES = @INCLUDES@

include_HEADERS = \
	$(top_builddir)/include/sigar.h \
	$(top_builddir)/include/sigar_log.h \
	$(top_builddir)/include/sigar_format.h \
	$(top_builddir)/include/sigar_fileinfo.h \
	$(top_builddir)/include/sigar_ptql.h

lib_LTLIBRARIES = libsigar.la

libsigar_la_LDFLAGS =

libsigar_la_LIBADD = $(top_builddir)/src/os/@SRC_OS@/libsigar_os.la

libsigar_la_CFLAGS =

libsigar_la_SOURCES = \
	$(include_HEADERS) \
	$(top_builddir)/include/sigar_private.h \
	$(top_builddir)/include/sigar_util.h \
	$(top_builddir)/include/sigar_getline.h \
	sigar.c \
	sigar_cache.c \
	sigar_fileinfo.c \
	sigar_format.c \
	sigar_getline.c \
	sigar_ptql.c \
	sigar_signal.c \
	sigar_util.c

