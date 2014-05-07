/*
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */

#include <stdio.h>
#include <stdlib.h>
#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_log.h"

/*
 * Can be used internally to dump out table index/values.
 */

static void rma_debug(sigar_rma_stat_handle_t *rma)
{
	int i;
	printf("Table element count   = %d\n", rma->element_count);
	printf("      current_pos     = %d\n", rma->current_pos);
	for (i = 0; i < rma->element_count; i++) {
		if (rma->samples[i].stime == 0) {
			continue;
		}
		printf("%-3d (%-5f) ", i, rma->samples[i].value);
		if (!((i + 1) % 10)) {
			printf("\n");
		}
	}
}

SIGAR_DECLARE(sigar_rma_stat_handle_t *)
sigar_rma_init(sigar_t *sigar, int max_average_time)
{
	if (max_average_time <= 0) {
		sigar_log_printf(sigar, SIGAR_LOG_ERROR,
				 "sigar_rma_init: invalid max_average_time : %d",
				 max_average_time);
		return NULL;
	}

	sigar_rma_stat_handle_t *rma;
	rma = calloc(1, sizeof(*rma));

	/* Allocate enough space to hold the longest period. */

	rma->element_count = max_average_time;

	rma->samples = calloc(rma->element_count, sizeof(rma_sample_t));
	rma->current_pos = 0;

	return rma;
}

/*
 * Add a sample and return the current 1m, 5m, 15m average
 */

SIGAR_DECLARE(void)
sigar_rma_add_fetch_std_sample(sigar_t *sigar, sigar_rma_stat_handle_t * rma,
		float value, sigar_int64_t cur_time_sec, sigar_loadavg_t *loadavg)
{
	sigar_rma_add_sample(sigar,  rma, value, cur_time_sec);

	loadavg->loadavg[0] = sigar_rma_get_average(sigar, rma,
			SIGAR_RMA_RATE_1_MIN, cur_time_sec);
	loadavg->loadavg[1] = sigar_rma_get_average(sigar, rma,
			SIGAR_RMA_RATE_5_MIN, cur_time_sec);
	loadavg->loadavg[2] = sigar_rma_get_average(sigar, rma,
			SIGAR_RMA_RATE_15_MIN, cur_time_sec);
}

/*
 * Add a sample and return the current averages.  Any number of averages may be
 * requested and the sample sizes are passed in the loadavg field.
 */

SIGAR_DECLARE(void)
sigar_rma_add_fetch_custom_sample(sigar_t *sigar,
		sigar_rma_stat_handle_t * rma, float value, sigar_int64_t cur_time_sec,
		sigar_loadavg_t *loadavg, int num_avg)
{
	int i;
	int avg_secs;
	sigar_rma_add_sample(sigar,  rma, value, cur_time_sec);

    for (i = 0; i < num_avg; i++) {
		avg_secs = loadavg->loadavg[i];
		loadavg->loadavg[i] = sigar_rma_get_average(sigar, rma, avg_secs,
				cur_time_sec);
	}
}

static int next_pos(sigar_rma_stat_handle_t * rma, int pos)
{
	pos++;
	if (pos >= rma->element_count) {
		pos = 0;
	}
	return pos;
}

static int prev_pos(sigar_rma_stat_handle_t * rma, int pos)
{
	pos--;
	if (pos < 0) {
		pos = rma->element_count - 1;
	}
	return pos;
}

SIGAR_DECLARE(void)
sigar_rma_add_sample(sigar_t *sigar, sigar_rma_stat_handle_t * rma,
		float value, sigar_int64_t cur_time_sec)
{
	if (rma == NULL) {
		sigar_log_printf(sigar, SIGAR_LOG_ERROR,
				"sigar_rma_add_sample: NULL sigar_rma_stat_handle_t");
		return;
	}

	rma_sample_t *sample = &rma->samples[rma->current_pos];

   	sample->value = value;

	if (cur_time_sec != 0) {
		sample->stime = cur_time_sec;
	} else {
		sample->stime = sigar_time_now_millis() / 1000;
	}

	rma->current_pos = next_pos(rma, rma->current_pos);
}

SIGAR_DECLARE(float)
sigar_rma_get_average(sigar_t *sigar, sigar_rma_stat_handle_t * rma, int rate,
		sigar_int64_t cur_time_sec)
{
	float			avg = 0;
	int				pos;
	int				count;
	rma_sample_t   *sample;

	if (rma == NULL) {
		sigar_log_printf(sigar, SIGAR_LOG_ERROR,
			"sigar_rma_get_average: NULL sigar_rma_stat_handle_t");
		return 0.0;
	}

	/* Start at our current position and work backwards. */
	pos = prev_pos(rma, rma->current_pos);
	count = 0;

	while (pos != rma->current_pos) {
		sample = &rma->samples[pos];

		if (sample->stime == 0 ||
			(cur_time_sec - sample->stime > rate)) {
				break;
		}

		avg += sample->value;
		count++;

		pos = prev_pos(rma, pos);
	}

	if (count == 0) {
		sigar_log_printf(sigar, SIGAR_LOG_ERROR,
			"sigar_rma_get_average: Computed 0 elements for rate : %d",
			rate);
		return 0.0;
	}

	return (avg / count);
}
