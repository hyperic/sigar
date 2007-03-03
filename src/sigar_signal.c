/*
 * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
 * This file is part of SIGAR.
 * 
 * SIGAR is free software; you can redistribute it and/or modify
 * it under the terms version 2 of the GNU General Public License as
 * published by the Free Software Foundation. This program is distributed
 * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA.
 */

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"

#ifdef WIN32
#include <windows.h>
#endif

#include <signal.h>
#include <errno.h>

SIGAR_DECLARE(int) sigar_proc_kill(sigar_pid_t pid, int signum)
{
#ifdef WIN32
    int status = -1;
    HANDLE proc =
        OpenProcess(PROCESS_ALL_ACCESS,
                    TRUE, (DWORD)pid);

    if (proc) {
        switch (signum) {
          case 0:
            status = SIGAR_OK;
            break;
          default:
            if (TerminateProcess(proc, signum)) {
                status = SIGAR_OK;
            }
            break;
        }

        CloseHandle(proc);

        if (status == SIGAR_OK) {
            return SIGAR_OK;
        }
    }
    return GetLastError();
#else
    if (kill(pid, signum) == -1) {
        return errno;
    }
    return SIGAR_OK;
#endif
}

SIGAR_DECLARE(int) sigar_signum_get(char *name)
{
    if (strnEQ(name, "SIG", 3)) {
        name += 3;
    }

    switch (*name) {
      case 'A':
#ifdef SIGABRT
        if (strEQ(name, "ABRT")) return SIGABRT;
#endif
#ifdef SIGALRM
        if (strEQ(name, "ALRM")) return SIGALRM;
#endif
        break;
      case 'B':
#ifdef SIGBUS
        if (strEQ(name, "BUS")) return SIGBUS;
#endif
        break;
      case 'C':
#ifdef SIGCONT
        if (strEQ(name, "CONT")) return SIGCONT;
#endif
#ifdef SIGCHLD
        if (strEQ(name, "CHLD")) return SIGCHLD;
#endif
#ifdef SIGCLD
        if (strEQ(name, "CLD")) return SIGCLD;
#endif
        break;
      case 'E':
#ifdef SIGEMT
        if (strEQ(name, "EMT")) return SIGEMT;
#endif
        break;
      case 'F':
#ifdef SIGFPE
        if (strEQ(name, "FPE")) return SIGFPE;
#endif
        break;
      case 'H':
#ifdef SIGHUP
        if (strEQ(name, "HUP")) return SIGHUP;
#endif
        break;
      case 'I':
#ifdef SIGINT
        if (strEQ(name, "INT")) return SIGINT;
#endif
#ifdef SIGILL
        if (strEQ(name, "ILL")) return SIGILL;
#endif
#ifdef SIGIOT
        if (strEQ(name, "IOT")) return SIGIOT;
#endif
#ifdef SIGIO
        if (strEQ(name, "IO")) return SIGIO;
#endif
#ifdef SIGINFO
        if (strEQ(name, "INFO")) return SIGINFO;
#endif
        break;
      case 'K':
#ifdef SIGKILL
        if (strEQ(name, "KILL")) return SIGKILL;
#endif
        break;
      case 'P':
#ifdef SIGPOLL
        if (strEQ(name, "POLL")) return SIGPOLL;
#endif
#ifdef SIGPIPE
        if (strEQ(name, "PIPE")) return SIGPIPE;
#endif
#ifdef SIGPROF
        if (strEQ(name, "PROF")) return SIGPROF;
#endif
#ifdef SIGPWR
        if (strEQ(name, "PWR")) return SIGPWR;
#endif
        break;
      case 'Q':
#ifdef SIGQUIT
        if (strEQ(name, "QUIT")) return SIGQUIT;
#endif
        break;
      case 'S':
#ifdef SIGSEGV
        if (strEQ(name, "SEGV")) return SIGSEGV;
#endif
#ifdef SIGSYS
        if (strEQ(name, "SYS")) return SIGSYS;
#endif
#ifdef SIGSTOP
        if (strEQ(name, "STOP")) return SIGSTOP;
#endif
#ifdef SIGSTKFLT
        if (strEQ(name, "STKFLT")) return SIGSTKFLT;
#endif
        break;
      case 'T':
#ifdef SIGTRAP
        if (strEQ(name, "TRAP")) return SIGTRAP;
#endif
#ifdef SIGTERM
        if (strEQ(name, "TERM")) return SIGTERM;
#endif
#ifdef SIGTSTP
        if (strEQ(name, "TSTP")) return SIGTSTP;
#endif
#ifdef SIGTTIN
        if (strEQ(name, "TTIN")) return SIGTTIN;
#endif
#ifdef SIGTTOU
        if (strEQ(name, "TTOU")) return SIGTTOU;
#endif
        break;
      case 'U':
#ifdef SIGURG
        if (strEQ(name, "URG")) return SIGURG;
#endif
#ifdef SIGUSR1
        if (strEQ(name, "USR1")) return SIGUSR1;
#endif
#ifdef SIGUSR2
        if (strEQ(name, "USR2")) return SIGUSR2;
#endif
        break;
      case 'V':
#ifdef SIGVTALRM
        if (strEQ(name, "VTALRM")) return SIGVTALRM;
#endif
        break;
      case 'W':
#ifdef SIGWINCH
        if (strEQ(name, "WINCH")) return SIGWINCH;
#endif
        break;
      case 'X':
#ifdef SIGXCPU
        if (strEQ(name, "XCPU")) return SIGXCPU;
#endif
#ifdef SIGXFSZ
        if (strEQ(name, "XFSZ")) return SIGXFSZ;
#endif
        break;
      default:
        break;
    }

    return -1;
}

