package gotoc

import (
	"fmt"
	"github.com/vmware/leap/util"
)

/*
#include "../../../../../Include/sigar.h"
*/
import "C"


type ProcStatInfo struct { 
	Total uint64
	Sleeping uint64
	Running uint64
	Zombie uint64
	Stopped uint64
	Idle uint64
	Threads uint64
}
func (this *ProcStatInfo) String() string { 
	return fmt.Sprintf("{Threads=%v, Sleeping=%v, Stopped=%v, Zombie=%v, Idle=%v, Total=%v, Running=%v}", 
		this.Threads, 
		this.Sleeping, 
		this.Stopped, 
		this.Zombie, 
		this.Idle, 
		this.Total, 
		this.Running,
	) 
}

func GetProcStatInfo() (procStatInfo *ProcStatInfo, err error) { 	
	defer util.Panic2Error(&err)
	var c_procStat C.sigar_proc_stat_t
	
	sigar := GetSigarHandle() 
	if status := int(C.sigar_proc_stat_get(sigar, &c_procStat)) ; status != SIGAR_OK { 
		return nil,fmt.Errorf("Failed to retrieve system proc stat info with error: %v", status) 
	}
	
	return &ProcStatInfo{
		Total : uint64(c_procStat.total),
		Sleeping : uint64(c_procStat.sleeping),
		Running : uint64(c_procStat.running),
		Zombie : uint64(c_procStat.zombie),
		Stopped : uint64(c_procStat.stopped),
		Idle : uint64(c_procStat.idle),
		Threads : uint64(c_procStat.threads),
	},nil
	
	
	
	 
	
	
}