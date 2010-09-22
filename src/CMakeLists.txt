## sigar has some base files + a set of platform specific files

MESSAGE(STATUS "CMAKE_SYSTEM_NAME is ${CMAKE_SYSTEM_NAME}")

INCLUDE_DIRECTORIES(../include/)

## linux
IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  SET(SIGAR_SRC os/linux/linux_sigar.c)

  INCLUDE_DIRECTORIES(os/linux/)
ENDIF(CMAKE_SYSTEM_NAME STREQUAL "Linux")

## macosx, freebsd
IF(CMAKE_SYSTEM_NAME MATCHES "(Darwin|FreeBSD)")
  SET(SIGAR_SRC os/darwin/darwin_sigar.c)

  INCLUDE_DIRECTORIES(os/darwin/)
  IF(CMAKE_SYSTEM_NAME MATCHES "(Darwin)")
    INCLUDE_DIRECTORIES(/Developer/Headers/FlatCarbon/)
    ADD_DEFINITIONS(-DDARWIN)
    SET(SIGAR_LINK_FLAGS "-framework CoreServices -framework IOKit")
  ELSE(CMAKE_SYSTEM_NAME MATCHES "(Darwin)")
    ## freebsd needs libkvm
    SET(SIGAR_LINK_FLAGS "-lkvm")
  ENDIF(CMAKE_SYSTEM_NAME MATCHES "(Darwin)")
ENDIF(CMAKE_SYSTEM_NAME MATCHES "(Darwin|FreeBSD)")

## solaris
IF (CMAKE_SYSTEM_NAME MATCHES "(Solaris|SunOS)" )
  SET(SIGAR_SRC
	os/solaris/solaris_sigar.c
	os/solaris/get_mib2.c
	os/solaris/kstats.c
	os/solaris/procfs.c
  )

  INCLUDE_DIRECTORIES(os/solaris/)
  ADD_DEFINITIONS(-DSOLARIS)
  SET(SIGAR_LINK_FLAGS -lkstat -ldl -lnsl -lsocket -lresolv)
ENDIF(CMAKE_SYSTEM_NAME MATCHES "(Solaris|SunOS)" )

## solaris
IF (CMAKE_SYSTEM_NAME MATCHES "(hpux)" )
  SET(SIGAR_SRC os/hpux/hpux_sigar.c)
  INCLUDE_DIRECTORIES(os/hpux/)
  ADD_DEFINITIONS(-DSIGAR_HPUX)
  SET(SIGAR_LINK_FLAGS -lnm)
ENDIF(CMAKE_SYSTEM_NAME MATCHES "(hpux)" )

## aix
IF (CMAKE_SYSTEM_NAME MATCHES "(AIX)" )
  SET(SIGAR_SRC os/aix/aix_sigar.c)

  INCLUDE_DIRECTORIES(os/aix/)
  SET(SIGAR_LINK_FLAGS -lodm -lcfg)
ENDIF(CMAKE_SYSTEM_NAME MATCHES "(AIX)" )

IF(WIN32)
  ADD_DEFINITIONS(-DSIGAR_SHARED)
  SET(SIGAR_SRC os/win32/peb.c os/win32/win32_sigar.c)
  INCLUDE_DIRECTORIES(os/win32)
ENDIF(WIN32)

SET(SIGAR_SRC ${SIGAR_SRC}
  sigar.c
  sigar_cache.c
  sigar_fileinfo.c
  sigar_format.c
  sigar_getline.c
  sigar_ptql.c
  sigar_signal.c
  sigar_util.c
)

ADD_LIBRARY(sigar SHARED ${SIGAR_SRC})
IF(WIN32)
	TARGET_LINK_LIBRARIES(sigar ws2_32 netapi32 version)
ENDIF(WIN32)
IF(SIGAR_LINK_FLAGS)
  SET_TARGET_PROPERTIES(sigar PROPERTIES LINK_FLAGS "${SIGAR_LINK_FLAGS}")
ENDIF(SIGAR_LINK_FLAGS)

INSTALL(TARGETS sigar
	RUNTIME DESTINATION bin
	ARCHIVE DESTINATION lib
	LIBRARY DESTINATION lib
	)
