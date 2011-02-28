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
#if defined(MSVC)
#include <WinError.h>
#endif

#include "sigar.h"
#include "sigar_private.h"
#include "sigar_format.h"
#include "sigar_tests.h"

#ifdef HAVE_VALGRIND_VALGRIND_H
#include <valgrind/valgrind.h>
#else
#define RUNNING_ON_VALGRIND 0 
#endif

TEST(test_sigar_proc_stat_get) {
	sigar_proc_stat_t proc_stat;

	assert(SIGAR_OK == sigar_proc_stat_get(t, &proc_stat));
	assert(proc_stat.total > 0);

	return 0;
}

TEST(test_sigar_proc_list_get) {
	sigar_proc_list_t proclist;
	size_t i;

	assert(SIGAR_OK == sigar_proc_list_get(t, &proclist));
	assert(proclist.number > 0);

	for (i = 0; i < proclist.number; i++) {
		sigar_pid_t pid = proclist.data[i];
		sigar_proc_mem_t proc_mem;
		sigar_proc_time_t proc_time;
		sigar_proc_state_t proc_state;
		int ret;

		if (SIGAR_OK == (ret = sigar_proc_mem_get(t, pid, &proc_mem))) {
			assert(IS_IMPL_U64(proc_mem.size));
			assert(IS_IMPL_U64(proc_mem.resident));
#if !(defined(SIGAR_TEST_OS_DARWIN) || defined(SIGAR_TEST_OS_SOLARIS) || defined(_WIN32))
			/* MacOS X, solaris nor win32 do provide them */
			assert(IS_IMPL_U64(proc_mem.share));
			assert(IS_IMPL_U64(proc_mem.minor_faults));
			assert(IS_IMPL_U64(proc_mem.major_faults));
#endif
#if !(defined(SIGAR_TEST_OS_DARWIN))
			/* freebsd */
			assert(IS_IMPL_U64(proc_mem.page_faults));
#endif
		} else {
			switch (ret) {
			case ESRCH:
			case EPERM:
				/* track the expected error code */
				break;
#if (defined(SIGAR_TEST_OS_DARWIN))
				/* valgrind on macosx doesn't handle this syscall yet */
			case ENOSYS:
				if (RUNNING_ON_VALGRIND) {
					break;
				}
#endif
			default:
				fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
				assert(ret == SIGAR_OK); 
				break;
			}
		}

		if (SIGAR_OK == (ret = sigar_proc_time_get(t, pid, &proc_time))) {
			assert(IS_IMPL_U64(proc_time.start_time));
			assert(IS_IMPL_U64(proc_time.user));
			assert(IS_IMPL_U64(proc_time.sys));
			assert(IS_IMPL_U64(proc_time.total));

#if !(defined(SIGAR_TEST_OS_DARWIN))
			/* Freebsd */
			assert(proc_time.start_time > 0);
#endif
			assert(proc_time.user >= 0);
			assert(proc_time.sys >= 0);
			assert(proc_time.total == proc_time.user + proc_time.sys);
		} else {
			switch (ret) {
			case EPERM:
			case ESRCH:
#if (defined(MSVC))
			/* OpenProcess() may return ERROR_ACCESS_DENIED */
			case ERROR_ACCESS_DENIED:
#endif
				/* track the expected error code */
				break;
#if (defined(SIGAR_TEST_OS_DARWIN))
				/* valgrind on macosx doesn't handle this syscall yet */
			case ENOSYS:
				if (RUNNING_ON_VALGRIND) {
					break;
				}
#endif
			default:
				fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
				assert(ret == SIGAR_OK); 
				break;
			}
		}
		if (SIGAR_OK == sigar_proc_state_get(t, pid, &proc_state)) {
			assert(proc_state.name != NULL);
#if 0
			/* all values are fine */
			(proc_state.state); /* we should check if the state is one of the announced group */
			(proc_state.ppid);
			(proc_state.tty);
			(proc_state.priority);
			(proc_state.nice);
			(proc_state.processor);
#endif
#if !(defined(SIGAR_TEST_OS_DARWIN) || defined(SIGAR_TEST_OS_LINUX))
			/* MacOS X doesn't provide them, Linux-IA64 neither */
			assert(IS_IMPL_U64(proc_state.threads));
#endif
		} else {
			switch (ret) {
				/* track the expected error code */
#if (defined(SIGAR_TEST_OS_DARWIN))
				/* valgrind on macosx doesn't handle this syscall yet */
			case ENOSYS:
				if (RUNNING_ON_VALGRIND) {
					break;
				}
#endif
			default:
				fprintf(stderr, "ret = %d (%s)\n", ret, sigar_strerror(t, ret));
				assert(ret == SIGAR_OK); 
				break;
			}
		}
	}

	sigar_proc_list_destroy(t, &proclist);

	return 0;
}

int main() {
	sigar_t *t;
	int err = 0;
	
	assert(SIGAR_OK == sigar_open(&t));

	test_sigar_proc_stat_get(t);
	test_sigar_proc_list_get(t);

	sigar_close(t);

	return err ? -1 : 0;
}
