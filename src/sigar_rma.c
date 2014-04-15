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
#include "sigar_log.h"
#include "sigar_rma.h"

sigar_rma_stat_handle_t *
sigar_rma_init(sigar_t *sigar, int sample_rate_secs, int max_average_time)
{
        if(sample_rate_secs <= 0)
        {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
                     "sigar_rma_init: invalid sample_rate_secs : %d", \
                     sample_rate_secs);
            return NULL;
        }

        if(max_average_time <= 0)
        {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
                     "sigar_rma_init: invalid max_average_time : %d", \
                     sample_rate_secs);
            return NULL;
        }

	sigar_rma_stat_handle_t *rma;
	rma = calloc(1, sizeof(*rma));

	/* Allocate enough space to hold the longest period. */

	rma->sample_rate_secs = sample_rate_secs;
	rma->element_count = max_average_time / sample_rate_secs;

	rma->values = calloc(rma->element_count, sizeof(float));
	rma->current_pos = 0;
	rma->have_wrapped = false;

	return rma;
}

void 
sigar_rma_add_sample(sigar_t *sigar, sigar_rma_stat_handle_t * rma, float sample)
{
        if(rma == NULL)
        {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
                     "sigar_rma_add_sample: NULL sigar_rma_stat_handle_t");
            return;
        }

	rma->values[rma->current_pos++] = sample;
	if (rma->current_pos == rma->element_count) {
		rma->have_wrapped = true;
		rma->current_pos = 0;
	}
}

float 
sigar_rma_get_average(sigar_t *sigar, sigar_rma_stat_handle_t * rma, int rate)
{
	float		avg = 0;
	int		pos;
	int		backup_size = rate / rma->sample_rate_secs;
	int		count;

        if(rma == NULL)
        {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
                     "sigar_rma_get_average: NULL sigar_rma_stat_handle_t");
            return 0.0;
        }

        /* Safety check to avoid divide by 0. */

        if(backup_size == 0)
        {
            sigar_log_printf(sigar, SIGAR_LOG_ERROR, \
                     "sigar_rma_get_average: Computed 0 backup for rate : %d and sample size %d",
                     rate, rma->sample_rate_secs);
            return 0.0;
        }

	/*
	 * * To compute the average, we first compute the number of samples
	 * to count based * on our sample rate, and how many samples/sec we
	 * are doing. * eg: (rate = RMA_RATE_1_MIN = 60) / (sample_rate_secs
	 * = 5) = 12 elements. * We then work backwards adding the number of
	 * elements, taking into account * buffer wrapping, and partial
	 * buffer (not yet having the total number of elements.)
	 */

	/*
	 * If our buffer has wrapped, then we are assured to have a full
	 * sample set.
	 */

	if (rma->have_wrapped) {
		for (count = 0, pos = rma->current_pos - 1; count < backup_size; count++, pos--) {
			if (pos < 0)
				pos = rma->element_count - 1;
			avg += rma->values[pos];
		}
	}
	/* We may or may not have a full sample set. */
	else {
		pos = rma->current_pos;

		if ((pos - backup_size) < 0) {
			/* We don't have a complete backup sample yet. */
			backup_size = pos;
		}
		for (count = backup_size; count > 0; count--) {
			avg += rma->values[pos - count];
		}
	}

	return (avg / backup_size);
}
