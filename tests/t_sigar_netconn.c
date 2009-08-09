/**
 * Copyright (c) 2009, Sun Microsystems Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - Neither the name of Sun Microsystems Inc. nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_format.h"
#include "sigar_tests.h"

TEST(test_sigar_net_connections_get) {
	sigar_net_connection_list_t connlist;
	size_t i;
	int ret;

	if (SIGAR_OK == (ret = sigar_net_connection_list_get(t, &connlist, 
				SIGAR_NETCONN_SERVER | SIGAR_NETCONN_CLIENT |
				SIGAR_NETCONN_TCP | SIGAR_NETCONN_UDP))) {
		assert(connlist.number > 0);

		for (i = 0; i < connlist.number; i++) {
			sigar_net_connection_t con = connlist.data[i];

			assert(con.local_port < 65536);
			assert(con.local_port < 65536);
			assert(con.uid >= 0);
			assert(con.inode >= 0);
			assert(con.type >= 0);
			assert(con.state >= 0);
			assert(con.send_queue >= 0);
			assert(con.receive_queue >= 0);
		}

		assert(SIGAR_OK == sigar_net_connection_list_destroy(t, &connlist));
	} else {
		switch (ret) {
		case 40013:  /* AIX: SIGAR_EPERM_KMEM */
			/* track the expected error code */
			break;
		default:
			fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
			assert(ret == SIGAR_OK); 
			break;
		}
	}

	return 0;
}

int main() {
	sigar_t *t;
	int err = 0;
	
	assert(SIGAR_OK == sigar_open(&t));

	test_sigar_net_connections_get(t);

	sigar_close(t);

	return err ? -1 : 0;
}
