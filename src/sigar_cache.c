#include "sigar.h"
#include "sigar_private.h"
#include "sigar_util.h"

/*
 * hash table to cache values where key is a unique number
 * such as:
 *  pid -> some process data
 *  uid -> user name
 *  gid -> group name
 */

#define ENTRIES_SIZE(n) \
    (sizeof(sigar_cache_entry_t *) * (n))

sigar_cache_t *sigar_cache_new(int size)
{
    sigar_cache_t *table = malloc(sizeof(*table));
    table->count = 0;
    table->size = size;
    table->entries = malloc(ENTRIES_SIZE(size));
    memset(table->entries, '\0', ENTRIES_SIZE(size));
    table->free_value = free;
    return table;
}

sigar_cache_entry_t *sigar_cache_get(sigar_cache_t *table,
                                     sigar_uint64_t key)
{
    sigar_cache_entry_t *entry, **ptr;

    for (ptr = table->entries + (key % table->size), entry = *ptr;
         entry; ptr = &entry->next, entry = *ptr)
    {
        if (entry->id == key) {
            return entry;
        }
    }

    if (table->count++ > table->size) {
        unsigned int new_size = table->size * 2;

        table->entries =
            realloc(table->entries, ENTRIES_SIZE(new_size));

        memset(table->entries + table->size, '\0',
               ENTRIES_SIZE(new_size - table->size));

        table->size = new_size;
    }

    *ptr = entry = malloc(sizeof(*entry));
    entry->id = key;
    entry->value = NULL;
    entry->next = NULL;

    return entry;
}

void sigar_cache_destroy(sigar_cache_t *table)
{
    int i;
    sigar_cache_entry_t **entries = table->entries;

    for (i=0; i<table->size; i++) {
        sigar_cache_entry_t *entry, *ptr;
        entry = ptr = *entries++;

        if (!entry) {
            continue;
        }

        do {
            if (ptr->value) {
                table->free_value(ptr->value);
            }
        } while ((ptr = ptr->next));
        free(entry);
    }

    free(table->entries);
}
