GCC = gcc
OTP_ROOT = $(shell ./check_otp -root)
EI_ROOT = $(shell ./check_otp -ei)
EI_LIB = $(EI_ROOT)/lib
EI_INC = $(EI_ROOT)/include
SIGAR_LIB = ../../java/sigar-bin/lib
LDFLAGS = -shared -L${EI_LIB} -lei -lpthread -L${SIGAR_LIB} -L../priv -lsigar
SRCS = sigar_drv.c
OUTPUT = ../priv/sigar_drv.so
CFLAGS = -Wall -fPIC -I../../../include -I${OTP_ROOT}/usr/include -I${EI_INC} -o ${OUTPUT}

all:
	${GCC} ${SRCS} ${LDFLAGS} ${CFLAGS}

clean:
	rm -rf ${OUTPUT} ../priv/gen/*.c ../priv/gen/*.hrl
