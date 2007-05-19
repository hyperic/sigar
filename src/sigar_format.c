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

/* Utility functions to provide string formatting of SIGAR data */

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>

int sigar_user_name_get(sigar_t *sigar, int uid, char *buf, int buflen)
{
    struct passwd *pw = NULL;
    /* XXX cache lookup */

# ifdef HAVE_GETPWUID_R
    struct passwd pwbuf;
    char buffer[512];

    if (getpwuid_r(uid, &pwbuf, buffer, sizeof(buffer), &pw) != 0) {
        return errno;
    }
    if (!pw) {
        return ENOENT;
    }
# else
    if ((pw = getpwuid(uid)) == NULL) {
        return errno;
    }
# endif

    strncpy(buf, pw->pw_name, buflen);
    buf[buflen-1] = '\0';

    return SIGAR_OK;
}

int sigar_group_name_get(sigar_t *sigar, int gid, char *buf, int buflen)
{
    struct group *gr;
    /* XXX cache lookup */

# ifdef HAVE_GETGRGID_R
    struct group grbuf;
    char buffer[512];

    if (getgrgid_r(gid, &grbuf, buffer, sizeof(buffer), &gr) != 0) {
        return errno;
    }
# else
    if ((gr = getgrgid(gid)) == NULL) {
        return errno;
    }
# endif

    if (gr && gr->gr_name) {
        strncpy(buf, gr->gr_name, buflen);
    }
    else {
        /* seen on linux.. apache httpd.conf has:
         * Group #-1
         * results in uid == -1 and gr == NULL.
         * wtf getgrgid_r doesnt fail instead? 
         */
        sprintf(buf, "%d", gid);
    }
    buf[buflen-1] = '\0';

    return SIGAR_OK;
}

int sigar_user_id_get(sigar_t *sigar, const char *name, int *uid)
{
    /* XXX cache lookup */
    struct passwd *pw;

# ifdef HAVE_GETPWNAM_R
    struct passwd pwbuf;
    char buf[512];

    if (getpwnam_r(name, &pwbuf, buf, sizeof(buf), &pw) != 0) {
        return errno;
    }
# else
    if (!(pw = getpwnam(name))) {
        return errno;
    }
# endif

    *uid = (int)pw->pw_uid;
    return SIGAR_OK;
}

static char *sigar_error_string(int err)
{
    switch (err) {
      case SIGAR_ENOTIMPL:
        return "This function has not been implemented on this platform";
      default:
        return "Error string not specified yet";
    }
}

SIGAR_DECLARE(char *) sigar_strerror(sigar_t *sigar, int err)
{
    char *buf;

    if (err < 0) {
        return sigar->errbuf;
    }

    if (err > SIGAR_OS_START_ERROR) {
        if ((buf = sigar_os_error_string(sigar, err)) != NULL) {
            return buf;
        }
        return "Unknown OS Error"; /* should never happen */
    }

    if (err > SIGAR_START_ERROR) {
        return sigar_error_string(err);
    }

    return sigar_strerror_get(err, sigar->errbuf, sizeof(sigar->errbuf));
}

char *sigar_strerror_get(int err, char *errbuf, int buflen)
{
    char *buf = NULL;
#ifdef WIN32
    DWORD len;

    len = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                        FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL,
                        err,
                        0, /* default language */
                        (LPTSTR)errbuf,
                        (DWORD)buflen,
                        NULL);
#else

#if defined(HAVE_STRERROR_R) && defined(HAVE_STRERROR_R_GLIBC)
    /*
     * strerror_r man page says:
     * "The GNU version may, but need not, use the user supplied buffer"
     */
    buf = strerror_r(err, errbuf, buflen);
#elif defined(HAVE_STRERROR_R)
    if (strerror_r(err, errbuf, buflen) < 0) {
        buf = "Unknown Error";
    }
#else
    /* strerror() is thread safe on solaris and hpux */
    buf = strerror(err);
#endif

    if (buf != NULL) {
        SIGAR_STRNCPY(errbuf, buf, buflen);
    }
    
#endif
    return errbuf;
}

void sigar_strerror_set(sigar_t *sigar, char *msg)
{
    SIGAR_SSTRCPY(sigar->errbuf, msg);
}

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

void sigar_strerror_printf(sigar_t *sigar, const char *format, ...)
{
    va_list args;

    va_start(args, format);
    vsnprintf(sigar->errbuf, sizeof(sigar->errbuf), format, args);
    va_end(args);
}

/* copy apr_strfsize */
SIGAR_DECLARE(char *) sigar_format_size(sigar_uint64_t size, char *buf)
{
    const char ord[] = "KMGTPE";
    const char *o = ord;
    int remain;

    if (size == SIGAR_FIELD_NOTIMPL) {
        buf[0] = '-';
        buf[1] = '\0';
        return buf;
    }

    if (size < 973) {
        sprintf(buf, "%3d ", (int) size);
        return buf;
    }

    do {
        remain = (int)(size & 1023);
        size >>= 10;

        if (size >= 973) {
            ++o;
            continue;
        }

        if (size < 9 || (size == 9 && remain < 973)) {
            if ((remain = ((remain * 5) + 256) / 512) >= 10) {
                ++size;
                remain = 0;
            }
            sprintf(buf, "%d.%d%c", (int) size, remain, *o);
            return buf;
        }

        if (remain >= 512) {
            ++size;
        }

        sprintf(buf, "%3d%c", (int) size, *o);

        return buf;
    } while (1);
}


SIGAR_DECLARE(int) sigar_uptime_string(sigar_t *sigar, 
                                       sigar_uptime_t *uptime,
                                       char *buffer,
                                       int buflen)
{
    char *ptr = buffer;
    int time = (int)uptime->uptime;
    int minutes, hours, days, offset = 0;

    /* XXX: get rid of sprintf and/or check for overflow */
    days = time / (60*60*24);

    if (days) {
        offset += sprintf(ptr + offset, "%d day%s, ",
                          days, (days > 1) ? "s" : "");
    }

    minutes = time / 60;
    hours = minutes / 60;
    hours = hours % 24;
    minutes = minutes % 60;

    if (hours) {
        offset += sprintf(ptr + offset, "%2d:%02d",
                          hours, minutes);
    }
    else {
        offset += sprintf(ptr + offset, "%d min", minutes);
    }

    return SIGAR_OK;
}


SIGAR_DECLARE(const char *)sigar_net_connection_type_get(int type)
{
    switch (type) {
      case SIGAR_NETCONN_TCP:
        return "tcp";
      case SIGAR_NETCONN_UDP:
        return "udp";
      case SIGAR_NETCONN_RAW:
        return "raw";
      case SIGAR_NETCONN_UNIX:
        return "unix";
      default:
        return "unknown";
    }
}

SIGAR_DECLARE(const char *)sigar_net_connection_state_get(int state)
{
    switch (state) {
      case SIGAR_TCP_ESTABLISHED:
        return "ESTABLISHED";
      case SIGAR_TCP_SYN_SENT:
        return "SYN_SENT";
      case SIGAR_TCP_SYN_RECV:
        return "SYN_RECV";
      case SIGAR_TCP_FIN_WAIT1:
        return "FIN_WAIT1";
      case SIGAR_TCP_FIN_WAIT2:
        return "FIN_WAIT2";
      case SIGAR_TCP_TIME_WAIT:
        return "TIME_WAIT";
      case SIGAR_TCP_CLOSE:
        return "CLOSE";
      case SIGAR_TCP_CLOSE_WAIT:
        return "CLOSE_WAIT";
      case SIGAR_TCP_LAST_ACK:
        return "LAST_ACK";
      case SIGAR_TCP_LISTEN:
        return "LISTEN";
      case SIGAR_TCP_CLOSING:
        return "CLOSING";
      case SIGAR_TCP_IDLE:
        return "IDLE";
      case SIGAR_TCP_BOUND:
        return "BOUND";
      case SIGAR_TCP_UNKNOWN:
      default:
        return "UNKNOWN";
    }
}

