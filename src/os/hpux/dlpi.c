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

/* 
 * talk to Data Link Provider Interface aka /dev/dlpi
 * see: http://docs.hp.com/hpux/onlinedocs/B2355-90139/B2355-90139.html
 */
#include <errno.h>
#include <fcntl.h>
#include <memory.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/dlpi.h>
#include <sys/dlpi_ext.h>
#include <sys/stat.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/mib.h>

#define	DLBUF_SIZE 8192

#define ERRBUF_SIZE 1024

static int send_req(int fd, char *ptr, int len, char *what, char *ebuf)
{
    struct strbuf ctl;
    int flags = 0;

    ctl.maxlen = 0;
    ctl.len    = len;
    ctl.buf    = ptr;

    if (putmsg(fd, &ctl, (struct strbuf *) NULL, flags) < 0) {
        snprintf(ebuf, ERRBUF_SIZE, "send_req: putmsg \"%s\": %s",
                 what, strerror(errno));
        return -1;
    }

    return 0;
}

static int recv_ack(int fd, int size, const char *what, char *bufp, char *ebuf)
{
    union DL_primitives *dlp;
    struct strbuf ctl;
    int flags = 0;

    ctl.maxlen = DLBUF_SIZE;
    ctl.len    = 0;
    ctl.buf    = bufp;

    if (getmsg(fd, &ctl, (struct strbuf*)NULL, &flags) < 0) {
        snprintf(ebuf, ERRBUF_SIZE, "recv_ack: %s getmsg: %s",
                 what, strerror(errno));
        return -1;
    }

    dlp = (union DL_primitives *)ctl.buf;
    switch (dlp->dl_primitive) {
      case DL_INFO_ACK:
      case DL_BIND_ACK:
      case DL_OK_ACK:
      case DL_HP_PPA_ACK:
      case DL_HP_INFO_ACK:
      case DL_GET_STATISTICS_ACK:
        break;

      case DL_ERROR_ACK:
        switch (dlp->error_ack.dl_errno) {

          case DL_SYSERR:
            snprintf(ebuf, ERRBUF_SIZE, "recv_ack: %s: system error - %s",
                     what, strerror(dlp->error_ack.dl_unix_errno));
            break;

          default:
            snprintf(ebuf, ERRBUF_SIZE, "recv_ack: %s: dl error - %d",
                     what, dlp->error_ack.dl_errno);
            break;
        }
        return -1;
      default:
        snprintf(ebuf, ERRBUF_SIZE,
                 "recv_ack: %s: unexpected primitive ack %d",
                 what, dlp->dl_primitive);
        return -1;
    }

    if (ctl.len < size) {
        snprintf(ebuf, ERRBUF_SIZE,
                 "recv_ack: %s: ack too small (%d < %d)",
                 what, ctl.len, size);
        return -1;
    }

    return ctl.len;
}

static int dl_attach_req(int fd, uint32_t ppa, char *ebuf)
{
    dl_attach_req_t req;

    req.dl_primitive = DL_ATTACH_REQ;
    req.dl_ppa = ppa;

    return send_req(fd, (char *)&req, sizeof(req), "attach", ebuf);
}

static int dl_bind_req(int fd, uint32_t sap, char *ebuf)
{
    dl_bind_req_t req;

    memset((char *)&req, 0, sizeof(req));
    req.dl_primitive = DL_BIND_REQ;

    req.dl_max_conind = 1;
    /* 22 == INSAP, see HP-UX DLPI Programmer's Guide */
    req.dl_sap = 22;
    req.dl_service_mode = DL_HP_RAWDLS;
    req.dl_service_mode = DL_CODLS;

    return send_req(fd, (char *)&req, sizeof(req), "bind", ebuf);
}

static int dl_bind_ack(int fd, char *bufp, char *ebuf)
{
    return recv_ack(fd, DL_BIND_ACK_SIZE, "bind", bufp, ebuf);
}

static int dl_ok_ack(int fd, const char *what, char *bufp, char *ebuf)
{
    return recv_ack(fd, DL_OK_ACK_SIZE, what, bufp, ebuf);
}

static int dl_info_req(int fd, char *ebuf)
{
    dl_info_req_t req;
    
    req.dl_primitive = DL_INFO_REQ;
    
    return send_req(fd, (char *)&req, sizeof(req), "info", ebuf);
}

static int dl_info_ack(int fd, char *bufp, char *ebuf)
{
    return recv_ack(fd, DL_INFO_ACK_SIZE, "info", bufp, ebuf);
}

static int dl_hp_info_req(int fd, char *ebuf)
{
    dl_hp_info_req_t req;

    req.dl_primitive = DL_HP_INFO_REQ;

    return send_req(fd, (char *)&req, sizeof(req), "hpinfo", ebuf);
}

static int dl_hp_info_ack(int fd, char *bufp, char *ebuf)
{
    return recv_ack(fd, DL_HP_INFO_ACK_SIZE, "hpinfo", bufp, ebuf);
}

static int dl_stats_req(int fd, char *ebuf)
{
    dl_get_statistics_req_t req;

    req.dl_primitive = DL_GET_STATISTICS_REQ;

    return send_req(fd, (char *)&req, sizeof(req), "stats", ebuf);
}

static int dl_stats_ack(int fd, char *bufp, char *ebuf)
{
    return recv_ack(fd, DL_GET_STATISTICS_ACK_SIZE, "stats", bufp, ebuf);
}

static int dl_open(int ppa, char *ebuf)
{
    char *dev = "/dev/dlpi";
    int fd = -1;
    dl_info_ack_t *infop;
    uint32_t buf[DLBUF_SIZE];
    char dname[128];

    if ((fd = open(dev, O_RDWR)) < 0) {
        snprintf(ebuf, sizeof(ebuf),
                 "failed to open %s: %s", dev, strerror(errno));
        return -1;
    }

    if (dl_info_req(fd, ebuf) < 0 ||
        dl_info_ack(fd, (char *)buf, ebuf) < 0) {
        return -1;
    }

    infop = &((union DL_primitives *)buf)->info_ack;

    if (infop->dl_provider_style == DL_STYLE2 &&
        (dl_attach_req(fd, ppa, ebuf) < 0 ||
         dl_ok_ack(fd, "attach", (char *)buf, ebuf) < 0))
    {
        return -1;
    }

    if (dl_bind_req(fd, 0, ebuf) < 0 ||
        dl_bind_ack(fd, (char *)buf, ebuf) < 0)
    {
        return -1;
    }

    return fd;
}

static int dl_get_hp_info(int fd, char *bufp, char *ebuf)
{
    dl_hp_info_ack_t *ip;

    if ((dl_hp_info_req(fd, ebuf) < 0) ||
        (dl_hp_info_ack(fd, bufp, ebuf) < 0)) {
        return -1;
    }

    ip = (dl_hp_info_ack_t *)bufp;
    if (ip->dl_primitive != DL_HP_INFO_ACK) {
        return -1;
    }

    return 0;
}

static int dl_get_stats(int fd, char *bufp, char *ebuf)
{
    dl_get_statistics_ack_t *ip;

    if ((dl_stats_req(fd, ebuf) < 0) ||
        (dl_stats_ack(fd, bufp, ebuf) < 0)) {
        return -1;
    }

    ip = (dl_get_statistics_ack_t *)bufp;
    if (ip->dl_primitive != DL_GET_STATISTICS_ACK) {
        return -1;
    }

    return 0;
}

int hpux_get_mib_ifentry(int ppa, mib_ifEntry *mib)
{
    int fd, status=0;
    char ebuf[ERRBUF_SIZE];
    uint32_t buf[DLBUF_SIZE];

   if ((fd = dl_open(ppa, ebuf)) < 0) {
        return errno;
    }

    if (dl_get_stats(fd, (char *)buf, ebuf) >= 0) {
        dl_get_statistics_ack_t *st = (dl_get_statistics_ack_t *)buf;
        memcpy(mib, (u_char *)buf + st->dl_stat_offset, sizeof(*mib));
    }
    else {
        status = errno;
    }

    close(fd);
    return status;
}
