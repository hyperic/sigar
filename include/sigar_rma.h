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

#ifndef SIGAR_RMA_H
#define SIGAR_RMA_H

#include <stdarg.h>
#include <stdbool.h>

#define SIGAR_RMA_RATE_1_MIN   1 * 60
#define SIGAR_RMA_RATE_5_MIN   5 * 60
#define SIGAR_RMA_RATE_15_MIN 15 * 60

typedef struct {

	/* Elements configured by the caller. */

	int		sample_rate_secs;	/* Period of sample secs.                 */
	int		element_count;	/* Number of elements in the ring
					 * buffer. */

	/* Internal items for tracking. */

	float          *values;	/* Ring buffer sample set.                */
	bool		have_wrapped;	/* Have we wrapped the buffer yet.        */
	int		current_pos;	/* Current index location.                */
} sigar_rma_stat_handle_t;

sigar_rma_stat_handle_t *sigar_rma_init(sigar_t *sigar, int sample_rate_secs, int max_average_time);
void		sigar_rma_add_sample(sigar_t *sigar, sigar_rma_stat_handle_t * rma, float sample);
float		sigar_rma_get_average(sigar_t *sigar, sigar_rma_stat_handle_t * rma, int rate);

#endif				/* SIGAR_RMA_H */
