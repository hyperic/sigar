package gotoc


import (
	
	
	
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"

type Info struct { 
	Free int64	
}

type MemInfo struct { 
	Mem *Info
	Swap *Info
}

func GetMemInfo() (*MemInfo, error){

	var sigar *C.sigar_t=GetSigarHandle()
	var mem C.sigar_mem_t
	var swap C.sigar_swap_t
	
	C.sigar_mem_get( sigar,  &mem)
	C.sigar_swap_get(sigar, &swap);
	
	return &MemInfo{ 
		Mem : &Info{ Free : int64(mem.free)}, 
		Swap : &Info{ Free : int64(swap.free)}, 
	},nil

    
	
}

   



