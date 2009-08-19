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

TEST(test_sigar_cpu_get) {
	sigar_cpu_t cpu;
	int ret;

	if (SIGAR_OK == (ret = sigar_cpu_get(t, &cpu))) {
		assert(IS_IMPL_U64(cpu.user));
		assert(IS_IMPL_U64(cpu.sys));
#if !(defined(SIGAR_TEST_OS_AIX))
		assert(IS_IMPL_U64(cpu.nice));
#endif
		assert(IS_IMPL_U64(cpu.idle));
		assert(IS_IMPL_U64(cpu.wait));
		assert(IS_IMPL_U64(cpu.total));
	} else {
		switch (ret) {
			/* track the expected error code */
		default:
			fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
			assert(ret == SIGAR_OK); 
			break;
		}
	}
	
	return 0;
}

TEST(test_sigar_cpu_list_get) {
	sigar_cpu_list_t cpulist;
	size_t i;
	int ret;

	if (SIGAR_OK != (ret = sigar_cpu_list_get(t, &cpulist))) {
		switch (ret) {
			/* track the expected error code */
		default:
			fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
			assert(ret == SIGAR_OK); 
			break;
		}
	}

	for (i = 0; i < cpulist.number; i++) {
		sigar_cpu_t cpu = cpulist.data[i];

		assert(IS_IMPL_U64(cpu.user));
		assert(IS_IMPL_U64(cpu.user));
		assert(IS_IMPL_U64(cpu.sys));
#if !(defined(SIGAR_TEST_OS_AIX))
		assert(IS_IMPL_U64(cpu.nice));
#endif
		assert(IS_IMPL_U64(cpu.idle));
		assert(IS_IMPL_U64(cpu.wait));
		assert(IS_IMPL_U64(cpu.total));
	}

	sigar_cpu_list_destroy(t, &cpulist);

	return 0;
}

TEST(test_sigar_cpu_info_get) {
	sigar_cpu_info_list_t cpuinfo;
	size_t i;

	assert(SIGAR_OK == sigar_cpu_info_list_get(t, &cpuinfo));

	for (i = 0; i < cpuinfo.number; i++) {
		sigar_cpu_info_t info = cpuinfo.data[i];

		assert(info.vendor);
		assert(info.model);
#if !(defined(SIGAR_TEST_OS_DARWIN))
		/* freebsd doesn't always expose it */
		assert(IS_IMPL_INT(info.mhz));
#endif
#if !(defined(SIGAR_TEST_OS_DARWIN) || defined(SIGAR_TEST_OS_SOLARIS) || defined(SIGAR_TEST_OS_HPUX) || defined(_WIN32))
		/* freebsd, solaris, hpux nor win32 do expose it */
		assert(IS_IMPL_U64(info.cache_size));
#endif
	}

	sigar_cpu_info_list_destroy(t, &cpuinfo);

	return 0;
}

int main() {
	sigar_t *t;
	int err = 0;
	
	assert(SIGAR_OK == sigar_open(&t));

	test_sigar_cpu_get(t);
	test_sigar_cpu_list_get(t);
	test_sigar_cpu_info_get(t);

	sigar_close(t);

	return err ? -1 : 0;
}
