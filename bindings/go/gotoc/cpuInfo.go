package gotoc

import (
	"fmt"
	"unsafe"
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"

type CpuInfo struct{ 
	Vendor string 
	Model string 
	Mhz int
	MhzMax int 
	MhzMin int 
	CacheSize uint 
	TotalSockets int
	TotalCores int 
	CorePerSocket int 
}

func GetCpuInfo()(result []*CpuInfo, err error){

	defer func() { 
		if r := recover() ; r != nil { 
			err = fmt.Errorf("Failed to rertieve cpu info due to: %v", err) 
		}
	}()
	var sigar *C.sigar_t=GetSigarHandle()
	
	var cpuInfoList C.sigar_cpu_info_list_t
	
	C.sigar_cpu_info_list_get(sigar, &cpuInfoList)
	
	var length int=int(cpuInfoList.number)
	
	cCpuInfo:=GetGoSlice(length, unsafe.Pointer(cpuInfoList.data))
	
	var goCpuInfo []C.sigar_cpu_info_t
	goCpuInfo= *(*[]C.sigar_cpu_info_t)(unsafe.Pointer(&cCpuInfo))
	
	C.sigar_cpu_info_list_destroy(sigar,&cpuInfoList)	
	
	result = make([]*CpuInfo, len(goCpuInfo))
	for i,sigarCpuinfo := range goCpuInfo  {
		
		result[i] = &CpuInfo{ 
			Vendor : C.GoString(&sigarCpuinfo.vendor[0]),
			Model : C.GoString(&sigarCpuinfo.model[0]), 
			Mhz : int(sigarCpuinfo.mhz), 
			MhzMax : int(sigarCpuinfo.mhz_max), 
			MhzMin : int(sigarCpuinfo.mhz_min),
			CacheSize : uint(sigarCpuinfo.cache_size),
			TotalSockets : int(sigarCpuinfo.total_sockets),
			TotalCores : int(sigarCpuinfo.total_cores), 
			CorePerSocket : int(sigarCpuinfo.cores_per_socket),
		}
	}
	
	return result,nil
	
}