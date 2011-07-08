#!/bin/sh

set -x

rm -f INSTALL NEWS \
&& rm -f -r m4 \
&& rm -f aclocal.m4 \
&& rm -f -r autom4te.cache \
&& rm -f compile \
&& rm -f config.guess \
&& rm -f config.log \
&& rm -f config.status \
&& rm -f config.sub \
&& rm -f configure \
&& rm -f depcomp \
&& rm -f src/config.h \
&& rm -f src/config.h.in \
&& rm -f install-sh \
&& rm -f -r libltdl \
&& rm -f libtool \
&& rm -f ltmain.sh \
&& rm -f Makefile \
&& rm -f Makefile.in \
&& rm -f missing \
&& rm -f src/*.la \
&& rm -f src/*.lo \
&& rm -f src/*.o \
&& rm -f src/sigar_version_autoconf.c \
&& rm -f -r src/.libs \
&& rm -f -r src/.deps \
&& rm -f src/os/Makefile \
&& rm -f src/os/Makefile.in \
&& rm -f src/os/*/Makefile \
&& rm -f src/os/*/Makefile.in \
&& rm -f src/os/*/*.la \
&& rm -f src/os/*/*.lo \
&& rm -f src/os/*/*.o \
&& rm -f -r src/os/*/.libs \
&& rm -f -r src/os/*/.deps \
&& rm -f src/Makefile \
&& rm -f src/Makefile.in \
&& rm -f src/stamp-h1 \
&& rm -f src/stamp-h1.in \
&& rm -f bindings/Makefile \
&& rm -f bindings/Makefile.in \
&& rm -f bindings/lua/Makefile \
&& rm -f bindings/*/Makefile.in \
&& rm -f -r bindings/*/.deps \
&& rm -f include/Makefile \
&& rm -f include/Makefile.in \
&& rm -f -r tests/.deps \
&& rm -f tests/Makefile \
&& rm -f tests/Makefile.in \
&& rm -f examples/Makefile \
&& rm -f examples/Makefile.in \
&& rm -f examples/*.o \
&& rm -f -r examples/.libs \
&& rm -f -r examples/.deps \
&& perl -le 's/\.c$// && unlink && print "rm $_" for <examples/*.c>'


