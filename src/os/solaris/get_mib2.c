/*
 * get_mib2() -- get MIB2 information from Solaris 2.[3-7] kernel
 *
 * V. Abell <abe@cc.purdue.edu>
 * Purdue University Computing Center
 */


/*
 * Copyright 1995 Purdue Research Foundation, West Lafayette, Indiana
 * 47907.  All rights reserved.
 *
 * Written by Victor A. Abell <abe@cc.purdue.edu>
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. Neither Victor A  Abell nor Purdue University are responsible for
 *    any consequences of the use of this software.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Credit to Victor A. Abell and Purdue
 *    University must appear in documentation and sources.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 *
 * 4. This notice may not be removed or altered.
 */

#if 0 /*ndef lint -Wall -Werror*/ 
static char copyright[] =
"@(#) Copyright 1995 Purdue Research Foundation.\nAll rights reserved.\n";
#endif

#include "get_mib2.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stropts.h>
#include <unistd.h>


/*
 * Local static values
 */

static char *Db = NULL;			/* data buffer */
static int Dbl = 0;			/* data buffer length */
static char *Smb = NULL;		/* stream message buffer */
static size_t Smbl = 0;			/* size of Smb[] */
static int Sd = -1;			/* stream device descriptor; not open
					 * if -1 */
static char ErrMsg[GET_MIB2_ERRMSGL];	/* error message buffer */


/*
 * close_mib2() - close MIB2 access
 *
 * return:
 *
 *	exit = GET_MIB2_OK if close succeeded
 *	       GET_MIB2_* is the error code for failure and:
 *			*errmsg = failure error message
 */

int
close_mib2(
	char **errmsg			/* error message buffer return
					 * address */
	)
{
	*errmsg = ErrMsg;
	if (Sd < 0) {
	    (void) strcpy(ErrMsg, "close_mib2: socket not open");
	    return(GET_MIB2_ERR_NOTOPEN);
	}
	if (close(Sd)) {
	    (void) sprintf(ErrMsg, "close_mib2: %s", strerror(errno));
	    return(GET_MIB2_ERR_CLOSE);
	}
	Sd = -1;
	if (Dbl && Db) {
	    Dbl = 0;
	    free((void *)Db);
	    Db = NULL;
	}
	if (Smbl && Smb) {
	    Smbl = 0;
	    free((void *)Smb);
	    Smb = NULL;
	}
	return(GET_MIB2_OK);
}


/*
 * get_mib2() - get MIB2 data
 *
 * return:
 *
 *	exit = GET_MIB2_OK if get succeeded, and:
 *			*opt = opthdr structure address
 *			*data = data buffer address
 *			*datalen = size of data buffer
 *	       GET_MIB2_* is the error code for failure and:
 *			*errmsg = failure error message
 */

int
get_mib2(
	struct opthdr **opt,		/* opthdr struct pointer return
					 * address */
	char **data,			/* data buffer return address */
	int *datalen,			/* data length return address */
	char **errmsg			/* error message buffer return
					 * address */
	)
{
	static struct T_optmgmt_ack *a;	/* message ACK pointer */
	static struct strbuf c;		/* streams control buffer */
	struct strbuf d;		/* streams data buffer */
	static struct T_error_ack *e;	/* message error pointer */
	int err;			/* error code */
	int f;				/* flags */
	static struct opthdr *o;	/* message option pointer */
	static struct T_optmgmt_req *r;	/* message request pointer */
	int rc;				/* reply code */

	*errmsg = ErrMsg;
/*
 * If MIB2 access isn't open, open it and issue the preliminary stream
 * messages.
 */
	if (Sd < 0) {

	/*
	 * Open access.  Return on error.
	 */
	    if ((err = open_mib2(errmsg)))
		return(err);
	/*
	 * Set up message request and option.
	 */
	    r = (struct T_optmgmt_req *)Smb;
	    o = (struct opthdr *)&Smb[sizeof(struct T_optmgmt_req)];
	    r->PRIM_type = T_OPTMGMT_REQ;
	    r->OPT_offset = sizeof(struct T_optmgmt_req);
	    r->OPT_length = sizeof(struct opthdr);

#if	defined(MI_T_CURRENT)
	    r->MGMT_flags = MI_T_CURRENT;
#else	/* !defined(MI_T_CURRENT) */
# if	defined(T_CURRENT)
	    r->MGMT_flags = T_CURRENT;
# else	/* !defined(T_CURRENT) */
#error	"Neither MI_T_CURRENT nor T_CURRENT are defined."
# endif	/* defined(T_CURRENT) */
#endif	/* defined(MI_T_CURRENT) */

	    o->level = MIB2_IP;
	    o->name = o->len = 0;
	    c.buf = Smb;
	    c.len = r->OPT_offset + r->OPT_length;
	/*
	 * Put the message.
	 */
	    if (putmsg(Sd, &c, (struct strbuf *)NULL, 0) == -1) {
		(void) sprintf(ErrMsg,
		    "get_mib2: putmsg request: %s", strerror(errno));
		return(GET_MIB2_ERR_PUTMSG);
	    }
	/*
	 * Set up to process replies.
	 */
	    a = (struct T_optmgmt_ack *)Smb;
	    c.maxlen = Smbl;
	    e = (struct T_error_ack *)Smb;
	    o = (struct opthdr *)&Smb[sizeof(struct T_optmgmt_ack)];
	}
/*
 * Get the next (first) reply message.
 */
	f = 0;
	if ((rc = getmsg(Sd, &c, NULL, &f)) < 0) {
	    (void) sprintf(ErrMsg, "get_mib2: getmsg(reply): %s",
		strerror(errno));
	    return(GET_MIB2_ERR_GETMSGR);
	}
/*
 * Check for end of data.
 */
	if (rc == 0
	&&  c.len >= sizeof(struct T_optmgmt_ack)
	&&  a->PRIM_type == T_OPTMGMT_ACK
	&&  a->MGMT_flags == T_SUCCESS
	&&  o->len == 0) {
	    err = close_mib2(errmsg);
	    if (err)
		return(err);
	    return(GET_MIB2_EOD);
	}
/*
 * Check for error.
 */
	if (c.len >= sizeof(struct T_error_ack)
	&&  e->PRIM_type == T_ERROR_ACK) {
	    (void) sprintf(ErrMsg,
		"get_mib2: T_ERROR_ACK: len=%d, TLI=%#x, UNIX=%#x",
		c.len, (int)e->TLI_error, (int)e->UNIX_error);
	    return(GET_MIB2_ERR_ACK);
	}
/*
 * Check for no data.
 */
	if (rc != MOREDATA
	||  c.len < sizeof(struct T_optmgmt_ack)
	||  a->PRIM_type != T_OPTMGMT_ACK
	||  a->MGMT_flags != T_SUCCESS) {
	    (void) sprintf(ErrMsg,
		"get_mib2: T_OPTMGMT_ACK: rc=%d len=%d type=%#x flags=%#x",
		rc, c.len, (int)a->PRIM_type, (int)a->MGMT_flags);
	    return(GET_MIB2_ERR_NODATA);
	}
/*
 * Allocate (or enlarge) the data buffer.
 */
	if (o->len >= Dbl) {
	    Dbl = o->len;
	    if (Db == NULL)
		Db = (char *)malloc(Dbl);
	    else
		Db = (char *)realloc(Db, Dbl);
	    if (Db == NULL) {
		(void) sprintf(ErrMsg,
		    "get_mib2: no space for %d byte data buffer", Dbl);
		return(GET_MIB2_ERR_NOSPC);
	    }
	}
/*
 * Get the data part of the message -- the MIB2 part.
 */
	d.maxlen = o->len;
	d.buf = Db;
	d.len = 0;
	f = 0;
	if ((rc = getmsg(Sd, NULL, &d, &f)) < 0) {
	    (void) sprintf(ErrMsg, "get_mib2: getmsg(data): %s",
		strerror(errno));
	    return(GET_MIB2_ERR_GETMSGD);
	}
	if (rc) {
	    (void) sprintf(ErrMsg,
		"get_mib2: getmsg(data): rc=%d maxlen=%d len=%d: %s",
		rc, d.maxlen, d.len, strerror(errno));
	    return(GET_MIB2_ERR_GETMSGD);
	}
/*
 * Compose a successful return.
 */
	*opt = o;
	*data = Db;
	*datalen = d.len;
	return(GET_MIB2_OK);
}


/*
 * open_mib2() - open access to MIB2 data
 *
 * return:
 *
 *	exit = GET_MIB2_OK if open succeeded
 *	       GET_MIB2_* is the error code for failure and:
 *			*errmsg = failure error message
 */

int
open_mib2(
	char **errmsg			/* error message buffer return
					 * address */
	)
{
	*errmsg = ErrMsg;
/*
 * It's an error if the stream device is already open.
 */
	if (Sd >= 0) {
	    (void) strcpy(ErrMsg, "open_mib2: MIB2 access already open");
	    return(GET_MIB2_ERR_OPEN);
	}
/*
 * Open the ARP stream device, push TCP and UDP on it.
 */
	if ((Sd = open(GET_MIB2_ARPDEV, O_RDWR, 0600)) < 0) {
	    (void) sprintf(ErrMsg, "open_mib2: %s: %s", GET_MIB2_ARPDEV,
		strerror(errno));
	    return(GET_MIB2_ERR_ARPOPEN);
	}
	if (ioctl(Sd, I_PUSH, GET_MIB2_TCPSTREAM) == -1) {
	    (void) sprintf(ErrMsg, "open_mib2: push %s: %s",
		GET_MIB2_TCPSTREAM, strerror(errno));
	    return(GET_MIB2_ERR_TCPPUSH);
	}
	if (ioctl(Sd, I_PUSH, GET_MIB2_UDPSTREAM) == -1) {
	    (void) sprintf(ErrMsg, "open_mib2: push %s: %s",
		GET_MIB2_UDPSTREAM, strerror(errno));
	    return(GET_MIB2_ERR_UDPPUSH);
	}
/*
 * Allocate a stream message buffer.
 */
	Smbl = sizeof(struct opthdr) + sizeof(struct T_optmgmt_req);
	if (Smbl < (sizeof (struct opthdr) + sizeof(struct T_optmgmt_ack)))
	    Smbl = sizeof (struct opthdr) + sizeof(struct T_optmgmt_ack);
	if (Smbl < sizeof(struct T_error_ack))
	    Smbl = sizeof(struct T_error_ack);
	if ((Smb = (char *)malloc(Smbl)) == NULL) {
	    (void) strcpy(ErrMsg,
		"open_mib2: no space for stream message buffer");
	    return(GET_MIB2_ERR_NOSPC);
	}
/*
 * All is OK.  Return that indication.
 */
	return(GET_MIB2_OK);
}
