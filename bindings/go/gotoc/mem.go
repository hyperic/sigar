package gotoc


import (
	"fmt"
	"github.com/vmware/leap/util"
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"

type Mem struct {
	Ram uint64 
	Total uint64
	Used uint64
	Free uint64
	ActualUsed uint64
	ActualFree uint64
	UsedPerecent float64
	FreePerecent float64
}
func (this *Mem) String() string { 
	return fmt.Sprintf("Mem: %vK av, %vK used, %vK free", (this.Total / 1024),(this.Used / 1024), (this.Free / 1024)) 
}

type Swap struct { 
	Total uint64
	Used uint64
	Free uint64
	PageIn uint64
	PageOut uint64
}
func (this *Swap) String() string { 
	return fmt.Sprintf("Swap: %vK av, %vK used, %vK free", (this.Total / 1024),(this.Used / 1024), (this.Free / 1024)) 
}

type MemInfo struct { 
	Mem *Mem
	Swap *Swap
}

func GetTotalMem() (total uint64,err error) { 
	defer util.Panic2Error(&err)
	var sigar *C.sigar_t=GetSigarHandle()
	var mem C.sigar_mem_t
	status := int(C.sigar_mem_get(sigar, &mem)) 
	if status != SIGAR_OK { 
		return 0,fmt.Errorf("Failed to rertieve sys mem info with error: %v", status)
	}
	return uint64(mem.total),nil 
}

func GetMemInfo() (result *MemInfo,err error){
	defer util.Panic2Error(&err)
	var sigar *C.sigar_t=GetSigarHandle()
	var mem C.sigar_mem_t
	var swap C.sigar_swap_t
	
	C.sigar_mem_get( sigar,  &mem)
	C.sigar_swap_get(sigar, &swap);
	
	return &MemInfo{ 
		Mem : &Mem{ 
			Ram : uint64(mem.ram),
			Total : uint64(mem.total),
			Used : uint64(mem.used),
			Free : uint64(mem.free),
			ActualUsed : uint64(mem.actual_used),
			ActualFree : uint64(mem.actual_free),
			UsedPerecent : float64(mem.used_percent),
			FreePerecent : float64(mem.free_percent),
		}, 
		Swap : &Swap{ 
			Total : uint64(swap.total),
			Used : uint64(swap.used),
			Free : uint64(swap.free),
			PageIn : uint64(swap.page_in),
			PageOut : uint64(swap.page_out),
		}, 
	},nil
	
}

   



