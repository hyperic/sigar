#include <stdio.h>
#include <sys/utsname.h>

int main(int argc, char **argv) {
    struct utsname name;
    uname(&name);

    printf("sysname....%s\n", name.sysname);
    printf("nodename...%s\n", name.nodename);
    printf("release....%s\n", name.release);
    printf("version....%s\n", name.version);
    printf("machine....%s\n", name.machine);

    return 0;
}
