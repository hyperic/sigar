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
