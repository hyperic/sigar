ERLC=erlc
EBIN=../ebin
INCLUDE=../include
EFLAGS=-I ${INCLUDE} -o ${EBIN}
SRCS=`ls *.erl`

all:
	${ERLC} ${EFLAGS} ${SRCS}

clean:
	rm -rf ${EBIN}/*.beam
