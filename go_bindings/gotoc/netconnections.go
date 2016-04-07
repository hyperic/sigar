package gotoc


import (
	
	
	"unsafe"
	
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"



func NetConections(flags int)(C.sigar_net_connection_list_t,
[]C.sigar_net_connection_t){

	var sigar *C.sigar_t=GetSigarHandle()
	var netlistconn C.sigar_net_connection_list_t
	
	
	
	C.sigar_net_connection_list_get(sigar, &netlistconn, C.int(flags))
	
	var length int=int(netlistconn.number)
	
	cconnection:=GetGoSlice(length, unsafe.Pointer(netlistconn.data))
	var goConnections []C.sigar_net_connection_t
	goConnections = *(*[]C.sigar_net_connection_t)(unsafe.Pointer(&cconnection))	
	
	
	
	C.sigar_net_connection_list_destroy(sigar, &netlistconn)
	
	
	
	
	return netlistconn, goConnections

    
	
}


