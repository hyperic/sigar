AC_INIT(libsigar, 1.6.2)
AC_CONFIG_SRCDIR(src/sigar.c)
AC_CONFIG_HEADERS(src/config.h)
AM_INIT_AUTOMAKE
AC_CANONICAL_HOST

AC_PROG_CC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL

AC_MSG_CHECKING([for os type ($host_os)])
case $host_os in
     *aix*)
     SRC_OS="aix"
     ;;
     *darwin*)
     SRC_OS="darwin"
     ;;
     *freebsd*)
     SRC_OS="darwin"
     LIBS="-lkvm"
     ;;
     *hpux*)
     SRC_OS="hpux"
     ;;
     *linux*)
     SRC_OS="linux"
     ;;
     *openbsd*)
     SRC_OS="darwin"
     LIBS="-lkvm"
     ;;
     *netbsd*)
     SRC_OS="darwin"
     LIBS="-lkvm"
     ;;
     *solaris*)
     SRC_OS="solaris"
     LIBS="-lkstat -lsocket"
     ;;
     *)
     ac_system="unknown"
esac
AC_MSG_RESULT([$ac_system])
AC_MSG_RESULT([$SRC_OS])

INCLUDES="-I\$(top_builddir)/include -I\$(srcdir)/os/$SRC_OS"

AC_SUBST(SRC_OS)
AC_SUBST(INCLUDES)
AC_SUBST(LIBS)

AC_CONFIG_FILES([
Makefile
src/Makefile
src/os/Makefile
src/os/aix/Makefile
src/os/darwin/Makefile
src/os/linux/Makefile
src/os/hpux/Makefile
src/os/solaris/Makefile
examples/Makefile
])

AC_OUTPUT
