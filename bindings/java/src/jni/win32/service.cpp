#ifdef WIN32
#define UNICODE
#define _UNICODE

#include "win32bindings.h"
#include "javasigar.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * This code is stolen from misc/win32/misc.c and apr_private.h
 * This helper code resolves late bound entry points 
 * missing from one or more releases of the Win32 API...
 * This is covered under the Apache Software License
 *
 * ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * Portions of this software are based upon public domain software
 * originally written at the National Center for Supercomputing Applications,
 * University of Illinois, Urbana-Champaign.
 */

typedef enum {
    DLL_WINBASEAPI = 0,    // kernel32 From WinBase.h
    DLL_WINADVAPI = 1,     // advapi32 From WinBase.h
    DLL_WINSOCKAPI = 2,    // mswsock  From WinSock.h
    DLL_WINSOCK2API = 3,   // ws2_32   From WinSock2.h
    DLL_SHSTDAPI = 4,      // shell32  From ShellAPI.h
    DLL_NTDLL = 5,         // shell32  From our real kernel
    DLL_defined = 6        // must define as last idx_ + 1
} dlltoken_e;

FARPROC load_dll_func(dlltoken_e fnLib, TCHAR* fnName, int ordinal);

#define DECLARE_LATE_DLL_FUNC(lib, rettype, calltype, fn, ord, args, names) \
typedef rettype (calltype *winapi_fpt_##fn) args; \
static winapi_fpt_##fn winapi_pfn_##fn = NULL; \
__inline rettype winapi_##fn args \
{ if (!winapi_pfn_##fn) \
winapi_pfn_##fn = (winapi_fpt_##fn) load_dll_func(lib, (wchar_t *)#fn, ord); \
return (*(winapi_pfn_##fn)) names; }; \

/* Win2K kernel only */
DECLARE_LATE_DLL_FUNC(DLL_WINADVAPI, BOOL, WINAPI, 
                      ChangeServiceConfig2W, 0,
                      (SC_HANDLE hService,
                       DWORD dwInfoLevel,
                       LPVOID lpInfo),
                      (hService, dwInfoLevel, lpInfo));
#undef ChangeServiceConfig2
#define ChangeServiceConfig2 winapi_ChangeServiceConfig2W

/* End Apache licensed code */

static TCHAR* lateDllName[] = {
    L"kernel32", L"advapi32", L"mswsock", L"ws2_32"  };
static HMODULE lateDllHandle[] = {
    NULL, NULL, NULL, NULL };

FARPROC load_dll_func(dlltoken_e fnLib, TCHAR* fnName, int ordinal)
{
    if (!lateDllHandle[fnLib]) {
        lateDllHandle[fnLib] = LoadLibrary(lateDllName[fnLib]);
        if (!lateDllHandle[fnLib]) {
            DWORD err = GetLastError();
            fprintf(stderr, "GetLastError():   %d  %s\r\n", 
                    err, lateDllName[fnLib]);
            return NULL;
        }
    }
    if (ordinal)
        return GetProcAddress(lateDllHandle[fnLib], (char *)ordinal);
    else
        return GetProcAddress(lateDllHandle[fnLib], (char *)fnName);
}

/*End ASF licensed code */

JNIEXPORT jboolean SIGAR_JNI(win32_Service_ChangeServiceDescription)
(JNIEnv *env, jclass, jlong handle, jstring description)
{
    jboolean            bResult = TRUE;
    SERVICE_DESCRIPTION servdesc;

    OSVERSIONINFO osver; /* VER_PLATFORM_WIN32_NT */
    osver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&osver);

    if ((osver.dwPlatformId == VER_PLATFORM_WIN32_NT) 
          && (osver.dwMajorVersion > 4) 
          && (ChangeServiceConfig2 != NULL)) {
        servdesc.lpDescription = (LPTSTR)env->GetStringChars(description, 
                                                             NULL);
        bResult = ChangeServiceConfig2((SC_HANDLE)handle, 
                                       SERVICE_CONFIG_DESCRIPTION, &servdesc);
        env->ReleaseStringChars(description, 
                                (const jchar *)servdesc.lpDescription);
    }
    return bResult;
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

JNIEXPORT jstring SIGAR_JNI(win32_Service_GetErrorMessage)
(JNIEnv *env, jclass, jint error)
{
    LPTSTR  lpMsg;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|
                  FORMAT_MESSAGE_IGNORE_INSERTS|
                  FORMAT_MESSAGE_FROM_SYSTEM,
                  NULL, error, 
                  MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US),
                  (LPTSTR)&lpMsg, 0, NULL);
    return env->NewString((const jchar *)lpMsg, lstrlen(lpMsg));
}

/**
 * XXX: this should probablly be moved into a util class
 */
JNIEXPORT jint SIGAR_JNI(win32_Service_GetLastError)
(JNIEnv *, jclass)
{
    return GetLastError();
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

#ifdef __cplusplus
}
#endif
#endif /* WIN32 */
