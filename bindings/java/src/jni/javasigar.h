#define JENV (*env)

#define SIGAR_PACKAGE "org/hyperic/sigar/"

#define SIGAR_JNI(m) JNICALL Java_org_hyperic_sigar_##m

#define SIGAR_JNIx(m) JNICALL Java_org_hyperic_sigar_Sigar_##m

#define SIGAR_FIND_CLASS(name) \
    JENV->FindClass(env, SIGAR_PACKAGE name)

#define SIGAR_CLASS_SIG(name) \
    "L" SIGAR_PACKAGE name ";"

