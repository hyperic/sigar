#include <jni.h>
#include "sigar.h"
#include "sigar_fileinfo.h"
#include "sigar_log.h"
#include "sigar_private.h"
#include "sigar_util.h"
#include "sigar_os.h"

#include <string.h>

#ifdef WIN32
#include <winsock.h>
#else
#include <errno.h>
#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>
#endif

#include "javasigar_generated.h"
#include "javasigar.h"

#if defined(__osf__) || defined(__LP64__)
#define SIGAR_POINTER_LONG
#endif

typedef struct {
    jclass classref;
    jfieldID *ids;
} jsigar_field_cache_t;

typedef struct {
    JNIEnv *env;
    jobject logger;
    sigar_t *sigar;
    jsigar_field_cache_t *fields[JSIGAR_FIELDS_MAX];
    int open_status;
    jthrowable not_impl;
} jni_sigar_t;

#define dSIGAR_GET \
    jni_sigar_t *jsigar = sigar_get_pointer(env, sigar_obj); \
    sigar_t *sigar

#define dSIGAR_VOID \
    dSIGAR_GET; \
    if (!jsigar) return; \
    sigar = jsigar->sigar; \
    jsigar->env = env

#define dSIGAR(val) \
    dSIGAR_GET; \
    if (!jsigar) return val; \
    sigar = jsigar->sigar; \
    jsigar->env = env

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *vm, void *reserved)
{
#ifdef DMALLOC
    char *options =
        getenv("DMALLOC_OPTIONS");
    if (!options) {
        options = 
            "debug=0x4f47d03,"
            "lockon=20,"
            "log=dmalloc-sigar.log";
    }
    dmalloc_debug_setup(options);
#endif
    return JNI_VERSION_1_2;
}

JNIEXPORT void JNICALL
JNI_OnUnload(JavaVM *vm, void *reserved)
{
#ifdef DMALLOC
    dmalloc_shutdown();
#endif
}

static void sigar_throw_exception(JNIEnv *env, char *msg)
{
    jclass errorClass = SIGAR_FIND_CLASS("SigarException");

    JENV->ThrowNew(env, errorClass, msg);
}

#define SIGAR_NOTIMPL_EX "SigarNotImplementedException"

static void sigar_throw_notimpl(JNIEnv *env, char *msg)
{
    jclass errorClass = SIGAR_FIND_CLASS(SIGAR_NOTIMPL_EX);

    JENV->ThrowNew(env, errorClass, msg);
}

#ifdef WIN32
#  define SIGAR_ENOENT ERROR_FILE_NOT_FOUND
#else
#  define SIGAR_ENOENT ENOENT
#endif

#ifdef WIN32
#  define SIGAR_EACCES ERROR_ACCESS_DENIED
#else
#  define SIGAR_EACCES EACCES
#endif

static void sigar_throw_error(JNIEnv *env, jni_sigar_t *jsigar, int err)
{
    jclass errorClass;

    switch (err) {
      case SIGAR_ENOENT:
        errorClass = SIGAR_FIND_CLASS("SigarFileNotFoundException");
        break;

      case SIGAR_EACCES:
        errorClass = SIGAR_FIND_CLASS("SigarPermissionDeniedException");
        break;

      case SIGAR_ENOTIMPL:
        if (jsigar->not_impl == NULL) {
            jfieldID id;
            jthrowable not_impl;

            errorClass = SIGAR_FIND_CLASS(SIGAR_NOTIMPL_EX);

            id = JENV->GetStaticFieldID(env, errorClass,
                                        "INSTANCE",
                                        SIGAR_CLASS_SIG(SIGAR_NOTIMPL_EX));

            not_impl = JENV->GetStaticObjectField(env, errorClass, id);

            jsigar->not_impl = JENV->NewGlobalRef(env, not_impl);
        }

        JENV->Throw(env, jsigar->not_impl);
        return;
      default:
        errorClass = SIGAR_FIND_CLASS("SigarException");
        break;
    }

    JENV->ThrowNew(env, errorClass,
                   sigar_strerror(jsigar->sigar, err));
}

static jni_sigar_t *sigar_get_pointer(JNIEnv *env, jobject obj) {
    jfieldID pointer_field;
    jni_sigar_t *jsigar;
    jclass cls;
      
    cls = JENV->GetObjectClass(env, obj);

#ifdef SIGAR_POINTER_LONG
    pointer_field = JENV->GetFieldID(env, cls, "longSigarWrapper", "J");
    jsigar = (jni_sigar_t *) JENV->GetLongField(env, obj, pointer_field);
#else
    pointer_field = JENV->GetFieldID(env, cls, "sigarWrapper", "I");
    jsigar = (jni_sigar_t *) JENV->GetIntField(env, obj, pointer_field);
#endif

    if (!jsigar) {
        sigar_throw_exception(env, "sigar has been closed");
        return NULL;
    }

    if (jsigar->open_status != SIGAR_OK) {
        sigar_throw_error(env, jsigar,
                          jsigar->open_status);
        return NULL;
    }

    return jsigar;
}

static void sigar_set_pointer(JNIEnv *env, jobject obj, const void *ptr) {
    jfieldID pointer_field;
    jclass cls = JENV->GetObjectClass(env, obj);

#ifdef SIGAR_POINTER_LONG
    pointer_field = JENV->GetFieldID(env, cls, "longSigarWrapper", "J");
    JENV->SetLongField(env, obj, pointer_field, (jlong)ptr);
#else
    pointer_field = JENV->GetFieldID(env, cls, "sigarWrapper", "I");
    JENV->SetIntField(env, obj, pointer_field, (int)ptr);
#endif
}

JNIEXPORT jstring SIGAR_JNI(Sigar_formatSize)
(JNIEnv *env, jclass cls, jlong size)
{
    char buf[56];
    sigar_format_size(size, buf);
    return JENV->NewStringUTF(env, buf);
}

JNIEXPORT void SIGAR_JNI(Sigar_open)
(JNIEnv *env, jobject obj)
{
    jni_sigar_t *jsigar = malloc(sizeof(*jsigar));

    memset(jsigar, '\0', sizeof(*jsigar));

    sigar_set_pointer(env, obj, jsigar);
        
    /* this method is called by the constructor.
     * if != SIGAR_OK save status and throw exception
     * when methods are invoked (see sigar_get_pointer).
     */
    if ((jsigar->open_status = sigar_open(&jsigar->sigar)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, jsigar->open_status);
        return;
    }
}

JNIEXPORT jint SIGAR_JNI(Sigar_nativeClose)
(JNIEnv *env, jobject sigar_obj)
{
    jint status;
    int i;
    dSIGAR(0);

    /* only place it is possible this would be something other than
     * SIGAR_OK is on win32 if RegCloseKey fails, which i don't think
     * is possible either.
     */
    status = sigar_close(sigar);

    if (jsigar->logger != NULL) {
        JENV->DeleteGlobalRef(env, jsigar->logger);
    }

    if (jsigar->not_impl != NULL) {
        JENV->DeleteGlobalRef(env, jsigar->not_impl);
    }

    for (i=0; i<JSIGAR_FIELDS_MAX; i++) {
        if (jsigar->fields[i]) {
            JENV->DeleteGlobalRef(env,
                                  jsigar->fields[i]->classref);
            free(jsigar->fields[i]->ids);
            free(jsigar->fields[i]);
        }
    }

    free(jsigar);
    sigar_set_pointer(env, sigar_obj, NULL);

    return status;
}

JNIEXPORT jlong SIGAR_JNI(Sigar_getPid)
(JNIEnv *env, jobject sigar_obj)
{
    dSIGAR(0);

    return sigar_pid_get(sigar);
}

JNIEXPORT void SIGAR_JNI(Sigar_kill)
(JNIEnv *env, jobject sigar_obj, jlong pid, jint signum)
{
    int status;
    dSIGAR_VOID;

    if ((status = sigar_proc_kill(pid, signum)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
    }
}

#define SetStringField(env, obj, fieldID, val) \
    SetObjectField(env, obj, fieldID, JENV->NewStringUTF(env, val))

static jstring jinet_ntoa(JNIEnv *env, sigar_t *sigar, sigar_uint64_t val) {
    char addr_str[SIGAR_INET_ADDR_LEN];
    sigar_inet_ntoa(sigar, val, addr_str);
    return JENV->NewStringUTF(env, addr_str);
}

#define SetNetAddrField(env, obj, fieldID, val) \
    SetObjectField(env, obj, fieldID, jinet_ntoa(env, sigar, val))

#include "javasigar_generated.c"

#define SIGAR_ALLOC_OBJECT(name) \
    JENV->AllocObject(env, SIGAR_FIND_CLASS(name))

enum {
    FS_FIELD_DIRNAME,
    FS_FIELD_DEVNAME,
    FS_FIELD_SYS_TYPENAME,
    FS_FIELD_TYPE,
    FS_FIELD_TYPENAME,
    FS_FIELD_MAX
};

#define STRING_SIG "Ljava/lang/String;"

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getFileSystemListNative)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    unsigned int i;
    sigar_file_system_list_t fslist;
    jobjectArray fsarray;
    jfieldID ids[FS_FIELD_MAX];
    jclass nfs_cls=NULL, cls = SIGAR_FIND_CLASS("FileSystem");
    dSIGAR(NULL);

    if ((status = sigar_file_system_list_get(sigar, &fslist)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    ids[FS_FIELD_DIRNAME] =
        JENV->GetFieldID(env, cls, "dirName", STRING_SIG);

    ids[FS_FIELD_DEVNAME] =
        JENV->GetFieldID(env, cls, "devName", STRING_SIG);

    ids[FS_FIELD_TYPENAME] =
        JENV->GetFieldID(env, cls, "typeName", STRING_SIG);

    ids[FS_FIELD_SYS_TYPENAME] =
        JENV->GetFieldID(env, cls, "sysTypeName", STRING_SIG);

    ids[FS_FIELD_TYPE] =
        JENV->GetFieldID(env, cls, "type", "I");

    fsarray = JENV->NewObjectArray(env, fslist.number, cls, 0);

    for (i=0; i<fslist.number; i++) {
        sigar_file_system_t *fs = &(fslist.data)[i];
        jobject fsobj;
        jclass obj_cls;

#ifdef WIN32
        obj_cls = cls;
#else
        if ((fs->type == SIGAR_FSTYPE_NETWORK) &&
            (strcmp(fs->sys_type_name, "nfs") == 0) &&
            strstr(fs->dev_name, ":/"))
        {
            if (!nfs_cls) {
                nfs_cls = SIGAR_FIND_CLASS("NfsFileSystem");
            }
            obj_cls = nfs_cls;
        }
        else {
            obj_cls = cls;
        }
#endif

        fsobj = JENV->AllocObject(env, obj_cls);

        JENV->SetStringField(env, fsobj,
                             ids[FS_FIELD_DIRNAME],
                             fs->dir_name);

        JENV->SetStringField(env, fsobj,
                             ids[FS_FIELD_DEVNAME],
                             fs->dev_name);

        JENV->SetStringField(env, fsobj,
                             ids[FS_FIELD_SYS_TYPENAME],
                             fs->sys_type_name);

        JENV->SetStringField(env, fsobj,
                             ids[FS_FIELD_TYPENAME],
                             fs->type_name);

        JENV->SetIntField(env, fsobj,
                          ids[FS_FIELD_TYPE],
                          fs->type);

        JENV->SetObjectArrayElement(env, fsarray, i, fsobj);
    }

    sigar_file_system_list_destroy(sigar, &fslist);

    return fsarray;
}

JNIEXPORT jboolean SIGAR_JNI(NfsFileSystem_ping)
(JNIEnv *env, jclass cls_obj, jstring jhostname)
{
#ifdef WIN32
    return JNI_FALSE; /*XXX*/
#else
    jboolean is_copy;
    const char *hostname;
    jboolean retval;

    if (!jhostname) {
        return JNI_FALSE;
    }

    hostname = JENV->GetStringUTFChars(env, jhostname, &is_copy);

    retval = (sigar_nfs_ping((char *)hostname) == SIGAR_OK);

    if (is_copy) {
        JENV->ReleaseStringUTFChars(env, jhostname, hostname);
    }

    return retval;
#endif
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getCpuInfoList)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    unsigned int i;
    sigar_cpu_info_list_t cpu_infos;
    jobjectArray cpuarray;
    jclass cls = SIGAR_FIND_CLASS("CpuInfo");
    dSIGAR(NULL);

    if ((status = sigar_cpu_info_list_get(sigar, &cpu_infos)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    JAVA_SIGAR_INIT_FIELDS_CPUINFO(cls);

    cpuarray = JENV->NewObjectArray(env, cpu_infos.number, cls, 0);

    for (i=0; i<cpu_infos.number; i++) {
        jobject info_obj = JENV->AllocObject(env, cls);
        JAVA_SIGAR_SET_FIELDS_CPUINFO(cls, info_obj,
                                      cpu_infos.data[i]);
        JENV->SetObjectArrayElement(env, cpuarray, i, info_obj);
    }

    sigar_cpu_info_list_destroy(sigar, &cpu_infos);

    return cpuarray;
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getCpuListNative)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    unsigned int i;
    sigar_cpu_list_t cpulist;
    jobjectArray cpuarray;
    jclass cls = SIGAR_FIND_CLASS("Cpu");
    dSIGAR(NULL);

    if ((status = sigar_cpu_list_get(sigar, &cpulist)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    JAVA_SIGAR_INIT_FIELDS_CPU(cls);

    cpuarray = JENV->NewObjectArray(env, cpulist.number, cls, 0);

    for (i=0; i<cpulist.number; i++) {
        jobject info_obj = JENV->AllocObject(env, cls);
        JAVA_SIGAR_SET_FIELDS_CPU(cls, info_obj,
                                  cpulist.data[i]);
        JENV->SetObjectArrayElement(env, cpuarray, i, info_obj);
    }

    sigar_cpu_list_destroy(sigar, &cpulist);

    return cpuarray;
}

JNIEXPORT jlongArray SIGAR_JNI(Sigar_getProcList)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    jlongArray procarray;
    sigar_proc_list_t proclist;
    jlong *pids = NULL;
    dSIGAR(NULL);

    if ((status = sigar_proc_list_get(sigar, &proclist)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    procarray = JENV->NewLongArray(env, proclist.number);

    if (sizeof(jlong) == sizeof(sigar_pid_t)) {
        pids = (jlong *)proclist.data;
    }
    else {
        unsigned int i;
        pids = (jlong *)malloc(sizeof(jlong) * proclist.number);

        for (i=0; i<proclist.number; i++) {
            pids[i] = proclist.data[i];
        }
    }

    JENV->SetLongArrayRegion(env, procarray, 0,
                             proclist.number, pids);

    if (pids != (jlong *)proclist.data) {
        free(pids);
    }

    sigar_proc_list_destroy(sigar, &proclist);

    return procarray;
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getProcArgs)
(JNIEnv *env, jobject sigar_obj, jlong pid)
{
    int status;
    unsigned int i;
    sigar_proc_args_t procargs;
    jobjectArray argsarray;
    jclass stringclass = JENV->FindClass(env, "java/lang/String");
    dSIGAR(NULL);

    if ((status = sigar_proc_args_get(sigar, pid, &procargs)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    argsarray = JENV->NewObjectArray(env, procargs.number, stringclass, 0);

    for (i=0; i<procargs.number; i++) {
        jstring s = JENV->NewStringUTF(env, procargs.data[i]);
        JENV->SetObjectArrayElement(env, argsarray, i, s);
    }

    sigar_proc_args_destroy(sigar, &procargs);

    return argsarray;
}

typedef struct {
    JNIEnv *env;
    jobject map;
    jmethodID id;
} jni_env_put_t;

static int jni_env_getall(void *data,
                          const char *key, int klen,
                          char *val, int vlen)
{
    jni_env_put_t *put = (jni_env_put_t *)data;
    JNIEnv *env = put->env;

    JENV->CallObjectMethod(env, put->map, put->id,  
                           JENV->NewStringUTF(env, key),
                           JENV->NewStringUTF(env, val));

    return SIGAR_OK;
}

JNIEXPORT jobject SIGAR_JNI(ProcEnv_getAll)
(JNIEnv *env, jobject cls, jobject sigar_obj, jlong pid)
{
    int status;
    sigar_proc_env_t procenv;
    jobject hashmap;
    jni_env_put_t put;
    jclass mapclass =
        JENV->FindClass(env, "java/util/HashMap");
    jmethodID mapid =
        JENV->GetMethodID(env, mapclass, "<init>", "()V");
    jmethodID putid =
        JENV->GetMethodID(env, mapclass, "put",
                          "(Ljava/lang/Object;Ljava/lang/Object;)"
                          "Ljava/lang/Object;");
    dSIGAR(NULL);

    hashmap = JENV->NewObject(env, mapclass, mapid);

    put.env = env;
    put.id = putid;
    put.map = hashmap;

    procenv.type = SIGAR_PROC_ENV_ALL;
    procenv.env_getter = jni_env_getall;
    procenv.data = &put;

    if ((status = sigar_proc_env_get(sigar, pid, &procenv)) != SIGAR_OK) {
        JENV->DeleteLocalRef(env, hashmap);
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    return hashmap;
}

typedef struct {
    JNIEnv *env;
    const char *key;
    int klen;
    jstring val;
} jni_env_get_t;

static int jni_env_getvalue(void *data,
                            const char *key, int klen,
                            char *val, int vlen)
{
    jni_env_get_t *get = (jni_env_get_t *)data;
    JNIEnv *env = get->env;

    if ((get->klen == klen) &&
        (strcmp(get->key, key) == 0))
    {
        get->val = JENV->NewStringUTF(env, val);
        return !SIGAR_OK; /* foundit; stop iterating */
    }

    return SIGAR_OK;
}

JNIEXPORT jstring SIGAR_JNI(ProcEnv_getValue)
(JNIEnv *env, jobject cls, jobject sigar_obj, jlong pid, jstring key)
{
    int status;
    sigar_proc_env_t procenv;
    jni_env_get_t get;
    dSIGAR(NULL);

    get.env = env;
    get.key = JENV->GetStringUTFChars(env, key, 0);
    get.klen = JENV->GetStringUTFLength(env, key);
    get.val = NULL;

    procenv.type = SIGAR_PROC_ENV_KEY;
    procenv.key  = get.key;
    procenv.klen = get.klen;
    procenv.env_getter = jni_env_getvalue;
    procenv.data = &get;

    if ((status = sigar_proc_env_get(sigar, pid, &procenv)) != SIGAR_OK) {
        JENV->ReleaseStringUTFChars(env, key, get.key);
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    JENV->ReleaseStringUTFChars(env, key, get.key);

    return get.val;
}

typedef struct {
    JNIEnv *env;
    jobject listobj;
    jmethodID id;
} jni_proc_module_t;

static int jni_proc_module_get(void *data,
                               char *name, int len)
{
    jni_proc_module_t *module = (jni_proc_module_t *)data;
    JNIEnv *env = module->env;

    JENV->CallBooleanMethod(env, module->listobj, module->id,  
                            JENV->NewStringUTF(env, name));

    return SIGAR_OK;
}

JNIEXPORT jobject SIGAR_JNI(Sigar_getProcModulesNative)
(JNIEnv *env, jobject sigar_obj, jlong pid)
{
    int status;
    sigar_proc_modules_t procmods;
    jobject listobj;
    jni_proc_module_t module;
    jclass listclass =
        JENV->FindClass(env, "java/util/ArrayList");
    jmethodID listid =
        JENV->GetMethodID(env, listclass, "<init>", "()V");
    jmethodID addid =
        JENV->GetMethodID(env, listclass, "add",
                          "(Ljava/lang/Object;)"
                          "Z");
    dSIGAR(NULL);

    listobj = JENV->NewObject(env, listclass, listid);

    module.env = env;
    module.id = addid;
    module.listobj = listobj;

    procmods.module_getter = jni_proc_module_get;
    procmods.data = &module;

    if ((status = sigar_proc_modules_get(sigar, pid, &procmods)) != SIGAR_OK) {
        JENV->DeleteLocalRef(env, listobj);
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    return listobj;
}

JNIEXPORT jdoubleArray SIGAR_JNI(Sigar_getLoadAverage)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    jlongArray avgarray;
    sigar_loadavg_t loadavg;
    dSIGAR(NULL);

    if ((status = sigar_loadavg_get(sigar, &loadavg)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    avgarray = JENV->NewDoubleArray(env, 3);

    JENV->SetDoubleArrayRegion(env, avgarray, 0,
                               3, loadavg.loadavg);

    return avgarray;
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getNetRouteList)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    unsigned int i;
    jarray routearray;
    jclass cls = SIGAR_FIND_CLASS("NetRoute");
    sigar_net_route_list_t routelist;
    dSIGAR(NULL);

    if ((status = sigar_net_route_list_get(sigar, &routelist)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    JAVA_SIGAR_INIT_FIELDS_NETROUTE(cls);

    routearray = JENV->NewObjectArray(env, routelist.number, cls, 0);

    for (i=0; i<routelist.number; i++) {
        jobject obj = JENV->AllocObject(env, cls);
        JAVA_SIGAR_SET_FIELDS_NETROUTE(cls, obj, routelist.data[i]);
        JENV->SetObjectArrayElement(env, routearray, i, obj);
    }

    sigar_net_route_list_destroy(sigar, &routelist);

    return routearray;
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getNetConnectionList)
(JNIEnv *env, jobject sigar_obj, jint flags)
{
    int status;
    unsigned int i;
    jarray connarray;
    jclass cls = SIGAR_FIND_CLASS("NetConnection");
    sigar_net_connection_list_t connlist;
    dSIGAR(NULL);

    status = sigar_net_connection_list_get(sigar, &connlist, flags);

    if (status != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    JAVA_SIGAR_INIT_FIELDS_NETCONNECTION(cls);

    connarray = JENV->NewObjectArray(env, connlist.number, cls, 0);

    for (i=0; i<connlist.number; i++) {
        jobject obj = JENV->AllocObject(env, cls);
        JAVA_SIGAR_SET_FIELDS_NETCONNECTION(cls, obj, connlist.data[i]);
        JENV->SetObjectArrayElement(env, connarray, i, obj);
    }

    sigar_net_connection_list_destroy(sigar, &connlist);

    return connarray;
}

JNIEXPORT jstring SIGAR_JNI(NetConnection_getTypeString)
(JNIEnv *env, jobject obj)
{
    jclass cls = JENV->GetObjectClass(env, obj);
    jfieldID field = JENV->GetFieldID(env, cls, "type", "I");
    jint type = JENV->GetIntField(env, obj, field);
    return JENV->NewStringUTF(env,
                              sigar_net_connection_type_get(type));
}

JNIEXPORT jstring SIGAR_JNI(NetConnection_getStateString)
(JNIEnv *env, jobject cls, jint state)
{
    return JENV->NewStringUTF(env,
                              sigar_net_connection_state_get(state));
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getWhoList)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    unsigned int i;
    sigar_who_list_t wholist;
    jobjectArray whoarray;
    jclass cls = SIGAR_FIND_CLASS("Who");
    dSIGAR(NULL);

    if ((status = sigar_who_list_get(sigar, &wholist)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    JAVA_SIGAR_INIT_FIELDS_WHO(cls);

    whoarray = JENV->NewObjectArray(env, wholist.number, cls, 0);

    for (i=0; i<wholist.number; i++) {
        jobject info_obj = JENV->AllocObject(env, cls);
        JAVA_SIGAR_SET_FIELDS_WHO(cls, info_obj,
                                  wholist.data[i]);
        JENV->SetObjectArrayElement(env, whoarray, i, info_obj);
    }

    sigar_who_list_destroy(sigar, &wholist);

    return whoarray;
}

/* XXX perhaps it would be better to duplicate these strings
 * in java land as static final so we dont create a new String
 * everytime.
 */
JNIEXPORT jstring SIGAR_JNI(FileInfo_getTypeString)
(JNIEnv *env, jclass cls, jint type)
{
    return JENV->NewStringUTF(env,
                              sigar_file_attrs_type_string_get(type));
}

JNIEXPORT jstring SIGAR_JNI(FileInfo_getPermissionsString)
(JNIEnv *env, jclass cls, jlong perms)
{
    char str[24];
    return JENV->NewStringUTF(env,
                              sigar_file_attrs_permissions_string_get(perms,
                                                                      str));
}

JNIEXPORT jint SIGAR_JNI(FileInfo_getMode)
(JNIEnv *env, jclass cls, jlong perms)
{
    return sigar_file_attrs_mode_get(perms);
}


/*
 * copy of the generated FileAttrs_gather function
 * but we call the lstat wrapper instead.
 */
JNIEXPORT void SIGAR_JNI(FileInfo_gatherLink)
(JNIEnv *env, jobject obj, jobject sigar_obj, jstring name)
{
    sigar_file_attrs_t s;
    int status;
    jclass cls = JENV->GetObjectClass(env, obj);
    const char *utf;
    dSIGAR_VOID;

    utf = JENV->GetStringUTFChars(env, name, 0);

    status = sigar_link_attrs_get(sigar, utf, &s);

    JENV->ReleaseStringUTFChars(env, name, utf);

    if (status != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return;
    }

    JAVA_SIGAR_INIT_FIELDS_FILEATTRS(cls);

    JAVA_SIGAR_SET_FIELDS_FILEATTRS(cls, obj, s);
}

JNIEXPORT jlong SIGAR_JNI(Sigar_getProcPort)
(JNIEnv *env, jobject sigar_obj, jint protocol, jlong port)
{
    int status;
    sigar_pid_t pid;
    dSIGAR(0);

    status = sigar_proc_port_get(sigar, protocol,
                                 (unsigned long)port, &pid);
    if (status != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return -1;
    }

    return pid;
}

JNIEXPORT jobjectArray SIGAR_JNI(Sigar_getNetInterfaceList)
(JNIEnv *env, jobject sigar_obj)
{
    int status;
    unsigned int i;
    sigar_net_interface_list_t iflist;
    jobjectArray ifarray;
    jclass stringclass = JENV->FindClass(env, "java/lang/String");
    dSIGAR(NULL);

    if ((status = sigar_net_interface_list_get(sigar, &iflist)) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    ifarray = JENV->NewObjectArray(env, iflist.number, stringclass, 0);

    for (i=0; i<iflist.number; i++) {
        jstring s = JENV->NewStringUTF(env, iflist.data[i]);
        JENV->SetObjectArrayElement(env, ifarray, i, s);
    }

    sigar_net_interface_list_destroy(sigar, &iflist);

    return ifarray;
}

JNIEXPORT jstring SIGAR_JNI(Sigar_getPasswordNative)
(JNIEnv *env, jclass classinstance, jstring prompt)
{
    const char *prompt_str;
    char *password;

    if (getenv("NO_NATIVE_GETPASS")) {
        sigar_throw_notimpl(env, "disabled with $NO_NATIVE_GETPASS");
        return NULL;
    }

    prompt_str = JENV->GetStringUTFChars(env, prompt, 0);

    password = sigar_password_get(prompt_str);

    JENV->ReleaseStringUTFChars(env, prompt, prompt_str);

    return JENV->NewStringUTF(env, password);
}

JNIEXPORT jstring SIGAR_JNI(Sigar_getFQDN)
(JNIEnv *env, jobject sigar_obj)
{
    char fqdn[SIGAR_FQDN_LEN];
    int status;
    dSIGAR(NULL);

    if ((status = sigar_fqdn_get(sigar, fqdn, sizeof(fqdn))) != SIGAR_OK) {
        sigar_throw_error(env, jsigar, status);
        return NULL;
    }

    return JENV->NewStringUTF(env, fqdn);
}

#include "sigar_getline.h"

JNIEXPORT jboolean SIGAR_JNI(util_Getline_isatty)
(JNIEnv *env, jclass cls)
{
    return isatty(fileno(stdin)) ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jstring SIGAR_JNI(util_Getline_getline)
(JNIEnv *env, jobject sigar_obj, jstring prompt)
{
    const char *prompt_str;
    char *line;
    jboolean is_copy;

    prompt_str = JENV->GetStringUTFChars(env, prompt, &is_copy);

    line = sigar_getline((char *)prompt_str);

    if (is_copy) {
        JENV->ReleaseStringUTFChars(env, prompt, prompt_str);
    }

    if ((line == NULL) ||
        sigar_getline_eof())
    {
        jclass eof_ex = JENV->FindClass(env, "java/io/EOFException");
        JENV->ThrowNew(env, eof_ex, "");
        return NULL;
    }

    return JENV->NewStringUTF(env, line);
}

JNIEXPORT void SIGAR_JNI(util_Getline_histadd)
(JNIEnv *env, jobject sigar_obj, jstring hist)
{
    const char *hist_str;
    jboolean is_copy;

    hist_str = JENV->GetStringUTFChars(env, hist, &is_copy);

    sigar_getline_histadd((char *)hist_str);

    if (is_copy) {
        JENV->ReleaseStringUTFChars(env, hist, hist_str);
    }
}

JNIEXPORT void SIGAR_JNI(util_Getline_histinit)
(JNIEnv *env, jobject sigar_obj, jstring hist)
{
    const char *hist_str;
    jboolean is_copy;

    hist_str = JENV->GetStringUTFChars(env, hist, &is_copy);

    sigar_getline_histinit((char *)hist_str);

    if (is_copy) {
        JENV->ReleaseStringUTFChars(env, hist, hist_str);
    }
}

static struct {
    JNIEnv *env;
    jobject obj;
    jmethodID id;
    jclass clazz;
} jsigar_completer;

static int jsigar_getline_completer(char *buffer, int offset, int *pos)
{
    JNIEnv *env = jsigar_completer.env;
    jstring jbuffer;
    jstring completion;
    const char *line;
    int len, cur;
    jboolean is_copy;

    jbuffer = JENV->NewStringUTF(env, buffer);

    completion = 
        JENV->CallObjectMethod(env, jsigar_completer.obj,
                               jsigar_completer.id, jbuffer);

    if (JENV->ExceptionOccurred(env)) {
        JENV->ExceptionDescribe(env);
        return 0;
    }

    if (!completion) {
        return 0;
    }

    line = JENV->GetStringUTFChars(env, completion, &is_copy);
    len = JENV->GetStringUTFLength(env, completion);

    cur = *pos;

    if (len != cur) {
        strcpy(buffer, line);
        *pos = len;
    }

    if (is_copy) {
        JENV->ReleaseStringUTFChars(env, completion, line);
    }

    return cur;
}

JNIEXPORT void SIGAR_JNI(util_Getline_setCompleter)
(JNIEnv *env, jclass classinstance, jobject completer)
{
    if (completer == NULL) {
        sigar_getline_completer_set(NULL);
        return;
    }
    
    jsigar_completer.env = env;
    jsigar_completer.obj = completer;
    jsigar_completer.clazz = JENV->GetObjectClass(env, completer);
    jsigar_completer.id =
        JENV->GetMethodID(env, jsigar_completer.clazz,
                          "complete",
                          "(Ljava/lang/String;)Ljava/lang/String;");

    sigar_getline_completer_set(jsigar_getline_completer);
}

JNIEXPORT void SIGAR_JNI(util_Getline_redraw)
(JNIEnv *env, jobject obj)
{
    sigar_getline_redraw();
}

JNIEXPORT void SIGAR_JNI(util_Getline_reset)
(JNIEnv *env, jobject obj)
{
    sigar_getline_reset();
}

static const char *log_methods[] = {
    "fatal", /* SIGAR_LOG_FATAL */
    "error", /* SIGAR_LOG_ERROR */
    "warn",  /* SIGAR_LOG_WARN */
    "info",  /* SIGAR_LOG_INFO */
    "debug", /* SIGAR_LOG_DEBUG */
    /* XXX trace is only in commons-logging??? */
    "debug", /* SIGAR_LOG_TRACE */
};

static void jsigar_log_impl(sigar_t *sigar, void *data,
                            int level, char *message)
{
    jni_sigar_t *jsigar = (jni_sigar_t *)data;
    JNIEnv *env = jsigar->env;
    jobject logger = jsigar->logger;
    jobject message_obj;

    /* XXX should cache method id lookups */
    jmethodID id =
        JENV->GetMethodID(env, JENV->GetObjectClass(env, logger),
                          log_methods[level],
                          "(Ljava/lang/Object;)V");

    if (JENV->ExceptionOccurred(env)) {
        JENV->ExceptionDescribe(env);
        return;
    }

    message_obj = (jobject)JENV->NewStringUTF(env, message);

    JENV->CallVoidMethod(env, logger, id, message_obj);
}

JNIEXPORT void SIGAR_JNI(SigarLog_setLogger)
(JNIEnv *env, jclass classinstance, jobject sigar_obj, jobject logger)
{
    dSIGAR_VOID;

    if (jsigar->logger != NULL) {
        JENV->DeleteGlobalRef(env, jsigar->logger);
        jsigar->logger = NULL;
    }

    if (logger) {
        jsigar->logger = JENV->NewGlobalRef(env, logger);

        sigar_log_impl_set(sigar, jsigar, jsigar_log_impl);
    }
    else {
        sigar_log_impl_set(sigar, NULL, NULL);
    }
}

JNIEXPORT void SIGAR_JNI(SigarLog_setLevel)
(JNIEnv *env, jclass classinstance, jobject sigar_obj, jint level)
{
    dSIGAR_VOID;

    sigar_log_level_set(sigar, level);
}

/*
 * XXX temporary function for ptql to map windows service to pid.
 * in the future would better to integrate win32bindings w/ sigar.
 */
#ifdef WIN32
typedef struct _SERVICE_STATUS_PROCESS {
    DWORD dwServiceType;
    DWORD dwCurrentState;
    DWORD dwControlsAccepted;
    DWORD dwWin32ExitCode;
    DWORD dwServiceSpecificExitCode;
    DWORD dwCheckPoint;
    DWORD dwWaitHint;
    DWORD dwProcessId;
    DWORD dwServiceFlags;
} SERVICE_STATUS_PROCESS;

typedef enum {
    SC_STATUS_PROCESS_INFO = 0
} SC_STATUS_TYPE;

typedef BOOL (CALLBACK *QueryServiceStatusExFunc)(SC_HANDLE,
                                                  SC_STATUS_TYPE,
                                                  LPBYTE,
                                                  DWORD,
                                                  LPDWORD);
#endif

JNIEXPORT jlong SIGAR_JNI(Sigar_getServicePid)
(JNIEnv *env, jobject sigar_obj, jstring jname)
{
#ifdef WIN32
    const char *name;
    jboolean is_copy;
    jlong pid = 0;
    DWORD err = ERROR_SUCCESS;
    SC_HANDLE mgr;
    dSIGAR(0);

    if (!sigar->adv_handle) {
        sigar_throw_notimpl(env, "advapi32.dll not available");
        return 0;
    }

    name = JENV->GetStringUTFChars(env, jname, &is_copy);

    mgr = OpenSCManager(NULL, SERVICES_ACTIVE_DATABASE,
                        SC_MANAGER_ALL_ACCESS);

    if (mgr) {
        HANDLE svc = OpenService(mgr, name, SERVICE_ALL_ACCESS);

        if (svc) {
            SERVICE_STATUS_PROCESS status;
            DWORD bytes;
            QueryServiceStatusExFunc status_query =
                (QueryServiceStatusExFunc)
                    GetProcAddress(sigar->adv_handle,
                                   "QueryServiceStatusEx");

            if (!status_query) {
                err = SIGAR_ENOTIMPL;
            }
            else if (status_query(svc,
                                  SC_STATUS_PROCESS_INFO,
                                  (LPBYTE)&status, sizeof(status), &bytes))
            {
                pid = status.dwProcessId;
            }
            else {
                err = GetLastError();
            }
            CloseServiceHandle(svc);
        }
        else {
            err = GetLastError();
        }

        CloseServiceHandle(mgr);
    }
    else {
        err = GetLastError();
    }

    if (is_copy) {
        JENV->ReleaseStringUTFChars(env, jname, name);
    }

    if (err != ERROR_SUCCESS) {
        sigar_throw_error(env, jsigar, err);
    }

    return pid;
#else
    dSIGAR(0);
    sigar_throw_error(env, jsigar, SIGAR_ENOTIMPL);
    return 0;
#endif
}

JNIEXPORT jlong SIGAR_JNI(ResourceLimit_INFINITY)
(JNIEnv *env, jclass cls)
{
#ifdef WIN32
    return 0x7fffffff;
#else
#include <sys/resource.h>
    return RLIM_INFINITY;
#endif
}
