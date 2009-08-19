SUBDIRS = os

INCLUDES = @INCLUDES@

lib_LTLIBRARIES = libsigar.la

libsigar_la_LDFLAGS = $(SIGAR_LIBS)

libsigar_la_LIBADD = $(top_builddir)/src/os/@SRC_OS@/libsigar_os.la

libsigar_la_CFLAGS = -I$(top_srcdir)/include

libsigar_la_SOURCES = \
	sigar.c \
	sigar_cache.c \
	sigar_fileinfo.c \
	sigar_format.c \
	sigar_getline.c \
	sigar_ptql.c \
	sigar_signal.c \
	sigar_util.c \
	sigar_version_autoconf.c

EXTRA_DIST=\
	sigar_version.c.in \
	CMakeLists.txt

