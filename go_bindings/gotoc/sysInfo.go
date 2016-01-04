package gotoc

import (
	
	
	"unsafe"
)

/*

#include "../../../../../Include/sigar.h"


 
#cgo CFLAGS:  -I../../../../../Include
#cgo LDFLAGS: C:/Users/iuriel/workspace/vmeerkat/leap-project/Include/sigar-amd64-winnt.dll
 
        
*/  
import "C"

func SysInfo() (C.sigar_sys_info_t, []C.sigar_who_t) {
	
	var sigar *C.sigar_t=GetSigarHandle()
	
	var sysInfo C.sigar_sys_info_t
    
    var wholist C.sigar_who_list_t 
    
    
    C.sigar_sys_info_get(sigar, &sysInfo)
    
    C.sigar_who_list_get(sigar,&wholist)
    
    var length int=int(wholist.number)
	
	var goWho =make([]C.sigar_who_t, length) 
    
    
    
	
	cwho:=GetGoSlice(length, unsafe.Pointer(wholist.data))
	
	goWho = *(*[]C.sigar_who_t)(unsafe.Pointer(&cwho))	
	
       
    C.sigar_who_list_destroy(sigar , &wholist)
      
	
	return sysInfo, goWho
	
}



