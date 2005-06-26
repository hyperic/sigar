#ifdef WIN32
#define UNICODE
#define _UNICODE

#include "win32bindings.h"
#include "javasigar.h"

#ifdef __cplusplus
extern "C" {
#endif

#define STRING_SIG "Ljava/lang/String;"

#define SERVICE_SetStringField(field, str) \
    id = env->GetFieldID(cls, field, STRING_SIG); \
    value = env->NewString((const jchar *)str, lstrlen(str)); \
    env->SetObjectField(obj, id, value)

#define SERVICE_SetIntField(field, val) \
    id = env->GetFieldID(cls, field, "I"); \
    env->SetIntField(obj, id, val)

typedef DWORD (CALLBACK *ChangeServiceConfig2_func_t)(SC_HANDLE,
                                                      DWORD, LPVOID);

typedef DWORD (CALLBACK *QueryServiceConfig2_func_t)(SC_HANDLE,
                                                     DWORD,
                                                     LPSERVICE_DESCRIPTION,
                                                     DWORD,
                                                     LPDWORD);

JNIEXPORT jboolean SIGAR_JNI(win32_Service_ChangeServiceDescription)
(JNIEnv *env, jclass, jlong handle, jstring description)
{
    jboolean result = FALSE;
    SERVICE_DESCRIPTION servdesc;
    HINSTANCE lib;
    ChangeServiceConfig2_func_t change_config;
    
    if ((lib = LoadLibrary(L"advapi32"))) {
        change_config = (ChangeServiceConfig2_func_t)
            GetProcAddress(lib, "ChangeServiceConfig2W");

        if (change_config) {
            servdesc.lpDescription =
                (LPTSTR)env->GetStringChars(description, NULL);

            result = change_config((SC_HANDLE)handle, 
                                   SERVICE_CONFIG_DESCRIPTION, &servdesc);
            env->ReleaseStringChars(description, 
                                    (const jchar *)servdesc.lpDescription);
        }

        FreeLibrary(lib);
    }

    return result;
}

JNIEXPORT jboolean SIGAR_JNI(win32_Service_CloseServiceHandle)
(JNIEnv *, jclass, jlong handle)
{
    return CloseServiceHandle((SC_HANDLE)handle);
}

JNIEXPORT jboolean SIGAR_JNI(win32_Service_ControlService)
(JNIEnv *, jclass, jlong handle, jint control)
{
    SERVICE_STATUS  status;
    return ControlService((SC_HANDLE)handle, control, &status);
}

JNIEXPORT jlong SIGAR_JNI(win32_Service_CreateService)
(JNIEnv *env,
 jclass,
 jlong handle,
 jstring serviceName,
 jstring displayName,
 jint serviceType,
 jint startType,
 jint errorControl,
 jstring path,
 jobjectArray dependencies, 
 jstring startName, 
 jstring password)
{
    TCHAR   szBuf[4048];
    LPCTSTR lpDepend = NULL;
    jlong   lResult;
    LPCTSTR lpStartName;

    LPCTSTR lpServiceName = (LPCTSTR)env->GetStringChars(serviceName, NULL);
    LPCTSTR lpDisplayName = (LPCTSTR)env->GetStringChars(displayName, NULL);
    LPCTSTR lpPath        = (LPCTSTR)env->GetStringChars(path, NULL);
    LPCTSTR lpPassword    = (LPCTSTR)env->GetStringChars(password, NULL);

    if(startName != NULL)
        lpStartName = (LPCTSTR)env->GetStringChars(path, NULL);
    else
        lpStartName = NULL;

    if(dependencies != NULL)
    {
        // Build a buffer of a double null terminated array of 
        // null terminated service names
        lpDepend             = szBuf;

        LPTSTR  lpBuf = szBuf;
        size_t  cbLen = 0;
        jsize   cSize = env->GetArrayLength(dependencies);

        for(int i = 0;i < cSize;i ++)
        {
            jstring str = (jstring)env->GetObjectArrayElement(dependencies, i);

            LPCTSTR lpStr = (LPCTSTR)env->GetStringChars(str, NULL);
            cbLen         = lstrlen(lpStr);

            // If we're going to overrun the buffer then break out of the loop
            if((lpBuf + cbLen + 1) >= (szBuf + sizeof(szBuf) / sizeof(TCHAR)))
                break;

            lstrcpy(lpBuf, lpStr);
            env->ReleaseStringChars(str, (const jchar *)lpStr);

            // Move the buffer to the byte beyond the current string 
            // null terminator
            lpBuf = lpBuf + cbLen + 1;
        }

        *lpBuf = 0;  // Double null terminate the string
    }

    // Create the Service
    lResult = (jlong)CreateService((SC_HANDLE)handle, lpServiceName,
                                   lpDisplayName, SERVICE_ALL_ACCESS,
                                   serviceType,
                                   startType, errorControl, lpPath, 
                                   NULL, NULL, lpDepend, lpStartName,
                                   lpPassword);

    if(lpStartName != NULL)
        env->ReleaseStringChars(path, (const jchar *)lpStartName);

    env->ReleaseStringChars(password, (const jchar *)lpPassword);
    env->ReleaseStringChars(path, (const jchar *)lpPath);
    env->ReleaseStringChars(displayName, (const jchar *)lpDisplayName);
    env->ReleaseStringChars(serviceName, (const jchar *)lpServiceName);

    return lResult;
}

JNIEXPORT jboolean SIGAR_JNI(win32_Service_DeleteService)
(JNIEnv *env, jclass, jlong handle)
{
    return DeleteService((SC_HANDLE)handle);
}

JNIEXPORT jlong SIGAR_JNI(win32_Service_OpenSCManager)
(JNIEnv *env, jclass, jstring machine, jint access)
{
    jlong   lResult;

    LPCTSTR lpMachine  = (LPCTSTR)env->GetStringChars(machine, NULL);
    lResult            = (jlong)OpenSCManager(lpMachine, NULL, access);
    env->ReleaseStringChars(machine, (const jchar *)lpMachine);

    return lResult;
}

JNIEXPORT jlong SIGAR_JNI(win32_Service_OpenService)
(JNIEnv *env, 
 jclass,
 jlong handle,
 jstring service,
 jint access)
{
    jlong   lResult;
    
    LPCTSTR lpService = (LPCTSTR)env->GetStringChars(service, NULL);
    lResult           = (jlong)OpenService((SC_HANDLE)handle, 
                                           lpService, access);
    env->ReleaseStringChars(service, (const jchar *)lpService);

    return lResult;
}

JNIEXPORT jint
SIGAR_JNI(win32_Service_QueryServiceStatus)
(JNIEnv *, jclass, jlong handle)
{
    SERVICE_STATUS status;
    int            iResult;

    if(QueryServiceStatus((SC_HANDLE)handle, &status) == TRUE) {
        iResult = status.dwCurrentState;
    } else
        iResult = -1;

    return iResult;
}

JNIEXPORT jboolean SIGAR_JNI(win32_Service_StartService)
(JNIEnv *, jclass, jlong handle)
{
    return StartService((SC_HANDLE)handle, 0, NULL);
}

JNIEXPORT jboolean SIGAR_JNI(win32_Service_StopService)
(JNIEnv *, jclass, jlong handle)
{
    SERVICE_STATUS  status;

    return ControlService((SC_HANDLE)handle, SERVICE_CONTROL_STOP, &status);
}

JNIEXPORT jobject SIGAR_JNI(win32_Service_getServiceNames)
(JNIEnv *env, jclass)
{
    SC_HANDLE handle =
        OpenSCManager(NULL, NULL, SC_MANAGER_ENUMERATE_SERVICE);
    ENUM_SERVICE_STATUS status, *services;
    BOOL retval;
    DWORD bytes, count, resume=0;
    DWORD type = SERVICE_WIN32, state = SERVICE_STATE_ALL;
    jobject listobj;
    jclass listclass =
        env->FindClass("java/util/ArrayList");
    jmethodID listid =
        env->GetMethodID(listclass, "<init>", "()V");
    jmethodID addid =
        env->GetMethodID(listclass, "add",
                         "(Ljava/lang/Object;)"
                         "Z");

    if (handle == NULL) {
        return NULL;
    }

    retval = EnumServicesStatus(handle, type, state,
                                &status, sizeof(status),
                                &bytes, &count, &resume);

    DWORD err = GetLastError();

    if ((retval == FALSE) || err == ERROR_MORE_DATA) {
        DWORD size = bytes + sizeof(ENUM_SERVICE_STATUS);
        services = new ENUM_SERVICE_STATUS[size];
        EnumServicesStatus(handle, type, state, services,
                           size, &bytes, &count, &resume);
    }

    listobj = env->NewObject(listclass, listid);
    for (int i=0; i<count; i++) {
        jstring name = 
            env->NewString((const jchar *)services[i].lpServiceName,
                           lstrlen(services[i].lpServiceName));
        env->CallBooleanMethod(listobj, addid, name);
    }

    CloseServiceHandle(handle);
    delete services;

    return listobj;
}

/*
 * convert:
 *   "RPCSS\0Tcpip\0IPSec\0\0"
 * to:
 *   ["RPCSS", "Tcpip", "IPSec"]
 */
static int to_array(JNIEnv *env, LPTSTR str, jobjectArray array)
{
    TCHAR *ptr = &str[0];
    int offset=0, i=0;

    while (*ptr != 0) {
        int slen = _tcslen(ptr);
        if (array) {
            jstring jstr =
                env->NewString((const jchar *)ptr, slen);
            env->SetObjectArrayElement(array, i, jstr);
        }
        offset += slen + 1;
        ptr = &str[offset];
        i++;
    }

    return i;
}

JNIEXPORT jboolean SIGAR_JNI(win32_Service_QueryServiceConfig)
(JNIEnv *env, jclass, jlong handle, jobject obj)
{
    char buffer[8096]; /* 8k is max size from mdsn docs */
    LPQUERY_SERVICE_CONFIG config = (LPQUERY_SERVICE_CONFIG)buffer;
    DWORD bytes;
    jfieldID id;
    jclass cls = env->GetObjectClass(obj);
    jstring value;
    HINSTANCE lib;

    if (!QueryServiceConfig((SC_HANDLE)handle, config,
                            sizeof(buffer), &bytes))
    {
        return JNI_FALSE;
    }

    SERVICE_SetIntField("type", config->dwServiceType);

    SERVICE_SetIntField("startType", config->dwStartType);

    SERVICE_SetIntField("errorControl", config->dwErrorControl);

    SERVICE_SetStringField("path", config->lpBinaryPathName);

    SERVICE_SetStringField("loadOrderGroup", config->lpLoadOrderGroup);

    SERVICE_SetIntField("tagId", config->dwTagId);

    if (config->lpDependencies) {
        /* first pass just get num for NewObjectArray */
        int num = to_array(env, config->lpDependencies, NULL);
        jclass stringclass =
            env->FindClass("java/lang/String");
        jobjectArray dependencies =
            env->NewObjectArray(num, stringclass, 0);

        to_array(env, config->lpDependencies, dependencies);

        id = env->GetFieldID(cls, "dependencies",
                             "[" STRING_SIG);

        env->SetObjectField(obj, id, dependencies);
    }

    SERVICE_SetStringField("serviceStartName", config->lpServiceStartName);

    SERVICE_SetStringField("displayName", config->lpDisplayName);

    if ((lib = LoadLibrary(L"advapi32"))) {
        LPSERVICE_DESCRIPTION desc = 
            (LPSERVICE_DESCRIPTION)buffer;
        QueryServiceConfig2_func_t query_config =
            (QueryServiceConfig2_func_t)
                GetProcAddress(lib, "QueryServiceConfig2W");

        if (query_config) {
            BOOL retval =
                query_config((SC_HANDLE)handle, 
                             SERVICE_CONFIG_DESCRIPTION,
                             desc, sizeof(buffer), &bytes);
            if (retval && (desc->lpDescription != NULL)) {
                SERVICE_SetStringField("description",
                                       desc->lpDescription);
            }
        }

        FreeLibrary(lib);
    }

    return JNI_TRUE;
}

#ifdef __cplusplus
}
#endif
#endif /* WIN32 */
