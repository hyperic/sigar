#ifndef SIGAR_UTIL_H
#define SIGAR_UTIL_H

/* most of this is crap for dealing with linux /proc */
#define UITOA_BUFFER_SIZE \
    (sizeof(int) * 3 + 1)

#define SSTRLEN(s) \
    (sizeof(s)-1)

#define sigar_strtoul(ptr) \
    strtoul(ptr, &ptr, 10)

#define sigar_isspace(c) \
    (isspace(((unsigned char)(c))))

#define sigar_isdigit(c) \
    (isdigit(((unsigned char)(c))))

#define sigar_isalpha(c) \
    (isalpha(((unsigned char)(c))))

#define PROC_FS_ROOT "/proc/"

char *sigar_uitoa(char *buf, unsigned int n, int *len);

SIGAR_INLINE char *sigar_skip_line(char *buffer, int buflen);

SIGAR_INLINE char *sigar_skip_token(char *p);

SIGAR_INLINE char *sigar_skip_multiple_token(char *p, int count);

int sigar_file2str(const char *fname, char *buffer, int buflen);

int sigar_proc_file2str(char *buffer, int buflen,
                        sigar_pid_t pid,
                        const char *fname,
                        int fname_len);

#define SIGAR_PROC_FILE2STR(buffer, pid, fname) \
    sigar_proc_file2str(buffer, sizeof(buffer), \
                        pid, fname, SSTRLEN(fname))

#define SIGAR_PROC_FILENAME(buffer, pid, fname) \
    sigar_proc_filename(buffer, sizeof(buffer), \
                        pid, fname, SSTRLEN(fname))

#define SIGAR_SKIP_SPACE(ptr) \
    while (sigar_isspace(*ptr)) ++ptr

char *sigar_proc_filename(char *buffer, int buflen,
                          sigar_pid_t pid,
                          const char *fname, int fname_len);

int sigar_proc_list_procfs_get(sigar_t *sigar,
                               sigar_proc_list_t *proclist);

int sigar_proc_fd_count(sigar_t *sigar, sigar_pid_t pid,
                        sigar_uint64_t *total);

/* generic util functions for all platforms */

int sigar_proc_count(sigar_t *sigar, sigar_uint64_t *total);

int sigar_mem_calc_ram(sigar_t *sigar, sigar_mem_t *mem);

double sigar_file_system_usage_calc_used(sigar_t *sigar,
                                         sigar_file_system_usage_t *fs);

void sigar_cpu_model_adjust(sigar_t *sigar, sigar_cpu_info_t *info);

typedef struct sigar_cache_entry_t sigar_cache_entry_t;

struct sigar_cache_entry_t {
    sigar_cache_entry_t *next;
    sigar_uint64_t id;
    void *value;
};

typedef struct {
    sigar_cache_entry_t **entries;
    unsigned int count, size;
    void (*free_value)(void *ptr);
} sigar_cache_t;

sigar_cache_t *sigar_cache_new(int size);

sigar_cache_entry_t *sigar_cache_get(sigar_cache_t *table,
                                     sigar_uint64_t key);

void sigar_cache_destroy(sigar_cache_t *table);

#endif /* SIGAR_UTIL_H */
