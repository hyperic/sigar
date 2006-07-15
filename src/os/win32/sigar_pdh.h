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

#ifndef SIGAR_PDH_H
#define SIGAR_PDH_H

/* performance data helpers */

#define PdhFirstObject(block) \
    ((PERF_OBJECT_TYPE *)((BYTE *) block + block->HeaderLength))

#define PdhNextObject(object) \
    ((PERF_OBJECT_TYPE *)((BYTE *) object + object->TotalByteLength))

#define PdhFirstCounter(object) \
    ((PERF_COUNTER_DEFINITION *)((BYTE *) object + object->HeaderLength))

#define PdhNextCounter(counter) \
    ((PERF_COUNTER_DEFINITION *)((BYTE *) counter + counter->ByteLength))

#define PdhGetCounterBlock(inst) \
    ((PERF_COUNTER_BLOCK *)((BYTE *) inst + inst->ByteLength))

#define PdhFirstInstance(object) \
    ((PERF_INSTANCE_DEFINITION *)((BYTE *) object + object->DefinitionLength))

#define PdhNextInstance(inst) \
    ((PERF_INSTANCE_DEFINITION *)((BYTE *)inst + inst->ByteLength + \
                                  PdhGetCounterBlock(inst)->ByteLength))

#define PdhInstanceName(inst) \
    ((wchar_t *)((BYTE *)inst + inst->NameOffset))

#endif /* SIGAR_PDH_H */
