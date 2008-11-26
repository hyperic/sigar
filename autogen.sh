#!/bin/sh
set -x

touch INSTALL NEWS AUTHORS

autoheader \
&& aclocal \
&& libtoolize --ltdl --copy --force \
&& automake --add-missing --copy \
&& autoconf
