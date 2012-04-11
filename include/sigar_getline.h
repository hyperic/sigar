#ifndef SIGAR_GETLINE_H
#define SIGAR_GETLINE_H

#include "sigar.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef int (*sigar_getline_completer_t)(char *, int, int *);

SIGAR_DECLARE(char *) sigar_getline(char *prompt);
SIGAR_DECLARE(void) sigar_getline_setwidth(int width);
SIGAR_DECLARE(void) sigar_getline_redraw(void);
SIGAR_DECLARE(void) sigar_getline_reset(void);
SIGAR_DECLARE(void) sigar_getline_windowchanged();
SIGAR_DECLARE(void) sigar_getline_histinit(char *file);
SIGAR_DECLARE(void) sigar_getline_histadd(char *buf);
SIGAR_DECLARE(int)  sigar_getline_eof();
SIGAR_DECLARE(void) sigar_getline_completer_set(sigar_getline_completer_t func);

#ifdef __cplusplus
}
#endif

#endif /* SIGAR_GETLINE_H */
