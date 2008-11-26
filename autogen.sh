#!/bin/sh
set -x

touch INSTALL NEWS AUTHORS
libtoolize="libtoolize"
if which glibtoolize >/dev/null 2>&1
then
    libtoolize=glibtoolize
fi

autoheader \
&& aclocal \
&& $libtoolize --copy --force \
&& automake --add-missing --copy \
&& autoconf
