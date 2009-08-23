INCLUDES=@INCLUDES@

lib_LTLIBRARIES=
TESTS=

if BUILD_LUA
lib_LTLIBRARIES += sigar.la

sigar_la_SOURCES =\
	sigar.c \
	sigar-cpu.c \
	sigar-disk.c \
	sigar-fs.c \
	sigar-mem.c \
	sigar-netif.c \
	sigar-proc.c \
	sigar-swap.c \
	sigar-sysinfo.c \
	sigar-version.c \
	sigar-who.c 

sigar_la_CPPFLAGS = ${LUA_CFLAGS}
sigar_la_LDFLAGS  = $(AM_LDFLAGS) -module -avoid-version
sigar_la_LIBADD  = $(top_builddir)/src/libsigar.la

TESTS += \
	sigar-test.lua

endif

noinst_HEADERS = lua-sigar.h

TESTS_ENVIRONMENT = \
	LUA_PATH="${srcdir}/?.lua" \
	LUA_CPATH="${builddir}/.libs/?.so" \
	DYLD_LIBRARY_PATH="${top_builddir}/src/.libs/" \
	lua 

