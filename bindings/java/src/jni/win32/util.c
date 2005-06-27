#ifdef WIN32

#include <pdh.h>
#include <pdhmsg.h>

#include "win32bindings.h"
#include "javasigar.h"

/**
 * Set of common utilities for all win32bindings objects
 */

#ifdef __cplusplus
extern "C" {
#endif

void win32_throw_exception(JNIEnv *env, char *msg)
{
    jclass exceptionClass = WIN32_FIND_CLASS("Win32Exception");
    JENV->ThrowNew(env, exceptionClass, msg);
}

void win32_throw_last_error(JNIEnv *env)
{
    win32_throw_error(env, GetLastError());
}

void win32_throw_error(JNIEnv *env, LONG err)
{
    char msg[8192];

    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,
                  err,
                  0, /* default language */
                  (LPTSTR)msg,
                  (DWORD)sizeof(msg),
                  NULL);

    win32_throw_exception(env, msg);
}

#ifdef __cplusplus
}
#endif
#endif /* WIN32 */
