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

TEST(test_sigar_net_iflist_get) {
	sigar_net_interface_list_t net_iflist;
	size_t i;
	int ret;

	assert(SIGAR_OK == sigar_net_interface_list_get(t, &net_iflist));
	assert(net_iflist.number > 0);

	for (i = 0; i < net_iflist.number; i++) {
		char *ifname = net_iflist.data[i];
		sigar_net_interface_stat_t   ifstat;
		sigar_net_interface_config_t config;

		if (SIGAR_OK == (ret = sigar_net_interface_stat_get(t, ifname, &ifstat))) {
#if defined(SIGAR_TEST_OS_SOLARIS)
			/* on solaris "lo" has no real stats, skip it */
			if (0 == strncmp(ifname, "lo", 2)) continue;
#endif
			assert(IS_IMPL_U64(ifstat.rx_packets));
			assert(IS_IMPL_U64(ifstat.rx_bytes));
			assert(IS_IMPL_U64(ifstat.rx_errors));
#if !(defined(SIGAR_TEST_OS_AIX))
			assert(IS_IMPL_U64(ifstat.rx_dropped));
#endif
#if !(defined(SIGAR_TEST_OS_DARWIN) || defined(SIGAR_TEST_OS_AIX) || defined(SIGAR_TEST_OS_HPUX) || defined(_WIN32))
			assert(IS_IMPL_U64(ifstat.rx_overruns));
			assert(IS_IMPL_U64(ifstat.rx_frame));
#endif
			assert(IS_IMPL_U64(ifstat.tx_packets));
			assert(IS_IMPL_U64(ifstat.tx_bytes));
			assert(IS_IMPL_U64(ifstat.tx_errors));
#if !(defined(SIGAR_TEST_OS_HPUX) || defined(_WIN32))
			assert(IS_IMPL_U64(ifstat.tx_collisions));
#endif
#if !(defined(SIGAR_TEST_OS_DARWIN) || defined(SIGAR_TEST_OS_AIX))
			assert(IS_IMPL_U64(ifstat.tx_dropped));
#endif
#if !(defined(SIGAR_TEST_OS_DARWIN) || defined(SIGAR_TEST_OS_AIX) || defined(SIGAR_TEST_OS_HPUX) || defined(_WIN32))
			assert(IS_IMPL_U64(ifstat.tx_overruns));
			assert(IS_IMPL_U64(ifstat.tx_carrier));
#endif
#ifndef SIGAR_TEST_OS_LINUX
			assert(IS_IMPL_U64(ifstat.speed));
#endif
		} else {
			switch (ret) {
				/* track the expected error code */
			default:
				fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
				assert(ret == SIGAR_OK); 
				break;
			}
		}

		if (SIGAR_OK == (ret = sigar_net_interface_config_get(t, ifname, &config))) {
			assert(config.name);
			assert(config.type);
			assert(config.description);
			assert(IS_IMPL_U64(config.flags));
			assert(IS_IMPL_U64(config.mtu));
			assert(IS_IMPL_U64(config.metric));
		} else {
			switch (ret) {
				/* track the expected error code */
			default:
				fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
				assert(ret == SIGAR_OK); 
				break;
			}
		}
	}

	assert(SIGAR_OK == sigar_net_interface_list_destroy(t, &net_iflist));

	return 0;
}

int main() {
	sigar_t *t;
	int err = 0;
	
	assert(SIGAR_OK == sigar_open(&t));

	test_sigar_net_iflist_get(t);

	sigar_close(t);

	return err ? -1 : 0;
}
