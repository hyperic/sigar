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

#ifdef __cplusplus
}
#endif
#endif /* WIN32 */
