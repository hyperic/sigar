#define JENV (*env)

#define SIGAR_PACKAGE "net/hyperic/sigar/"

#define SIGAR_JNI(m) JNICALL Java_net_hyperic_sigar_##m

#define SIGAR_FIND_CLASS(name) \
    JENV->FindClass(env, SIGAR_PACKAGE name)

#define SIGAR_CLASS_SIG(name) \
    "L" SIGAR_PACKAGE name ";"

