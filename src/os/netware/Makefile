NDKBASE	= c:/novell
#SYS:/ drive mount
NDRIVE = g:

NDK_ROOT = $(NDKBASE)/ndk
SDK_LIBC = $(NDK_ROOT)/libc

JBINDINGS = ../../../bindings/java
JBUILD    = $(JBINDINGS)/build
DEF_FILE  = $(JBUILD)/sigar.def
JINCLUDES = -I$(NDRIVE)/JAVA/include -I$(NDRIVE)/JAVA/include/netware
INCLUDES  = $(JINCLUDES) -I../../../include -I. -I$(JBUILD)/src
OBJDIR    = $(JBUILD)/obj/x86-netware
LIBDIR    = $(JBINDINGS)/sigar-bin/lib
SIGARLIB  = $(LIBDIR)/sigar.nlm

DB	= NDEBUG
# DB	= DEBUG

ifeq ($(DB),NDEBUG)
	OPT	= -O2
else
	OPT	= -g
endif

CC = mwccnlm

CFLAGS	= -DNETWARE -D$(DB) -nostdinc $(OPT)
CFLAGS	+= -gccinc -inline off -opt nointrinsics -align 4 -inst mmx -proc 686

PRELUDE = $(SDK_LIBC)/imports/libcpre.o
LD	= mwldnlm
LDFLAGS	= -nostdlib $(PRELUDE) $(DEF_FILE) $(NW_OBJ) $(SIGAR_OBJ) $(JNI_OBJ) -o $(SIGARLIB) -type generic

INCLUDES += \
-I$(SDK_LIBC)/include \
-I$(SDK_LIBC)/include/nks \
-I$(SDK_LIBC)/include/winsock

CFLAGS += $(INCLUDES)

NW_SRC = .\netware_sigar.c

SIGAR_SRC = \
..\..\sigar.c \
..\..\sigar_cache.c \
..\..\sigar_getline.c \
..\..\sigar_fileinfo.c \
..\..\sigar_util.c \
..\..\sigar_win32ish.c \

JNI_SRC = $(JBINDINGS)\src\jni\javasigar.c

NW_OBJ    = $(NW_SRC:.c=.o)
SIGAR_OBJ = $(SIGAR_SRC:.c=.o)
JNI_OBJ   = $(JNI_SRC:.c=.o)

all: $(OBJDIR) $(SIGARLIB)

clean:
	-$(RM) -r $(OBJDIR)

$(OBJDIR):
	@mkdir $(OBJDIR)

$(SIGARLIB): $(SIGAR_OBJ) $(NW_OBJ) $(JNI_OBJ) $(DEF_FILE)
	@echo Linking $@
	@-$(RM) $@
	@$(LD) $(LDFLAGS)
