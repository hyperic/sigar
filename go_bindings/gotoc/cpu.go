package gotoc


import (
	
	
	"unsafe"	
)


//#include "../../../../../Include/sigar.h"
import "C" 

     
     
  


func Cpu()  (C.sigar_cpu_list_t,[]C.sigar_cpu_t){ 
 
 
	
	var sigar *C.sigar_t=GetSigarHandle()
	
	var cpulists C.sigar_cpu_list_t
	
	C.sigar_cpu_list_get(sigar, &cpulists)
	
	var length int=int(cpulists.number)
	
	
	usp:=GetGoSlice(length, unsafe.Pointer(cpulists.data))
	
	var goCpu []C.sigar_cpu_t
	goCpu = *(*[]C.sigar_cpu_t)(unsafe.Pointer(&usp))	
	
	
	C.sigar_cpu_list_destroy(sigar, &cpulists)
  
    
    
    return cpulists, goCpu
	
	
}



 



