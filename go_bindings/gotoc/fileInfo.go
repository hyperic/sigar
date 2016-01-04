package gotoc


import (
	
	
	"unsafe"
)
/*
#include "../../../../../Include/sigar.h"



*/
import "C"


func FileInfo() []C.sigar_file_system_t{

	var sigar *C.sigar_t=GetSigarHandle()
	var fileSystemList C.sigar_file_system_list_t 
	//C.fileInfo(sigar,&fileSystemList)
	C.sigar_file_system_list_get(sigar, &fileSystemList);
	
	var length int=int(fileSystemList.number)
	
	
	cFs:=GetGoSlice(length, unsafe.Pointer(fileSystemList.data))
	
	var goFs []C.sigar_file_system_t
	goFs = *(*[]C.sigar_file_system_t)(unsafe.Pointer(&cFs))	
	 
	//fmt.Printf("%v\n", C.GoString(&goFs[1].dir_name[0])) 
	C.sigar_file_system_list_destroy(sigar, &fileSystemList);
	
	return goFs

    
	
}

   



