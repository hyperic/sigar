#ifndef WIN32_BINDINGS_H
#define WIN32_BINDINGS_H

/* Exclude rarely-used stuff from windows headers */
#define WIN32_LEAN_AND_MEAN

/* Windows Header Files */
#include <windows.h>

#include <tchar.h>

/* Include java jni headers */
#include <jni.h>

#define WIN32_PACKAGE "net/covalent/win32bindings/"

#define WIN32_FIND_CLASS(name) \
    JENV->FindClass(env, WIN32_PACKAGE name)

#define WIN32_ALLOC_OBJECT(name) \
    JENV->AllocObject(env, WIN32_FIND_CLASS(name))

#define SetStringField(env, obj, fieldID, val) \
    JENV->SetObjectField(env, obj, fieldID, JENV->NewStringUTF(env, val))

void win32_throw_exception(JNIEnv *env, char *msg);

#endif
