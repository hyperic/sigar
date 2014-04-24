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
#include "sigar_rma.h"

sigar_rma_stat_handle_t *
sigar_rma_init(sigar_t *sigar, int max_average_time)
{
        if(max_average_time <= 0)
        {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
                     "sigar_rma_init: invalid max_average_time : %d", \
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

void
sigar_rma_add_sample(sigar_t *sigar, sigar_rma_stat_handle_t * rma, float value, sigar_int64_t cur_time)
{
	if(rma == NULL)
	{
		sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
				"sigar_rma_add_sample: NULL sigar_rma_stat_handle_t");
		return;
	}

	rma_sample_t *sample = &rma->samples[rma->current_pos++];

   	sample->value = value;

	if(cur_time != 0)
		sample->stime = cur_time;
	else
		sample->stime = sigar_time_now_millis() / 1000;

	if (rma->current_pos == rma->element_count) {
		rma->current_pos = 0;
	}
}

float
sigar_rma_get_average(sigar_t *sigar, sigar_rma_stat_handle_t * rma, int rate, sigar_int64_t cur_time)
{
	float			avg = 0;
	int				pos;
	int				count;
	rma_sample_t   *sample;

	if(rma == NULL)
	{
		sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
			"sigar_rma_get_average: NULL sigar_rma_stat_handle_t");
		return 0.0;
	}

	/* Start at our current position and work backwards. */

	pos = rma->current_pos - 1;
	count = 0;

	while(pos != rma->current_pos) {
		sample = &rma->samples[pos];

		if ( sample->stime == 0 ||
			(cur_time - sample->stime > rate)) {
				break;
		}

		avg += sample->value;
		count++;
		pos--;

		if(pos < 0)
			pos = rma->element_count - 1;
	}

	if(count == 0)
	{
		sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
			"sigar_rma_get_average: Computed 0 elements for rate : %d",
			rate);
			return 0.0;
	}

	return (avg / count);
}
