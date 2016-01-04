package gotoc

import (
	

	
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"

func Threads(id int64)C.sigar_thread_cpu_t{

	var threadCpu C.sigar_thread_cpu_t
	
	var sigar *C.sigar_t=GetSigarHandle()
	
	C.sigar_thread_cpu_get(sigar , C.sigar_uint64_t(id) ,&threadCpu)

	return threadCpu	
}







