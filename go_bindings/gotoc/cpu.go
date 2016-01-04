package gotoc


import (
	"github.com/vmware/leap/util"
	"unsafe"	
	"fmt"
	"time"
	"strconv"
)


//#include "../../../../../Include/sigar.h"
//#include "../../../../../Include/sigar_format.h"
import "C" 

var _prevCpuUsage C.sigar_cpu_t //usged for percentage calculation 
var prevSampleTs int64
    
type CpuUsageInfo struct { 
	User uint64
	Sys uint64
	Nice uint64
	Idle uint64
	Wait uint64
	Irq uint64
	SoftIrq uint64
	Stolen uint64
	Total uint64
}  

type CpuPctUsageInfo struct { 
	User float64
	Sys float64
	Nice float64
	Idle float64
	Wait float64
	Irq float64
	SoftIrq float64
	Stolen float64
	Combined float64
}  
func (this *CpuPctUsageInfo) String() string {  
	return "CPU states: "+
			strconv.FormatFloat(this.User,'f', 1, 64)+"% user, "+
			strconv.FormatFloat(this.Sys,'f', 1, 64)+"% system, "+
			strconv.FormatFloat(this.Nice,'f', 1, 64)+"% nice, "+
			strconv.FormatFloat(this.Wait,'f', 1, 64) +"% wait, "+
			strconv.FormatFloat(this.Idle,'f', 1, 64)+"% idle"
}

//------------------------------------------------------------------------------------------------------------------------------------------------
// CpuPctUsageInfo (Float64) CpuPctUsageInfo (Float64) CpuPctUsageInfo (Float64) CpuPctUsageInfo (Float64) CpuPctUsageInfo (Float64) 
//------------------------------------------------------------------------------------------------------------------------------------------------
func GetCpuPctUsageInfo() (result *CpuPctUsageInfo, err error) { 
	defer util.Panic2Error(&err)
	
	sigar := GetSigarHandle()
	cur_c_cpu_t, err := _getCpu(sigar) 
	if err != nil { 
		return nil,err 
	}

	if prevSampleTs == 0 { 
		
		_prevCpuUsage = *cur_c_cpu_t
		time.Sleep(500*time.Millisecond) 
		
		cur_c_cpu_t, err = _getCpu(sigar) 
		if err != nil { 
			return nil,err 
		}
	}
	
	var c_pct_cpu_t C.sigar_cpu_perc_t
	C.sigar_cpu_perc_calculate(&_prevCpuUsage, cur_c_cpu_t, &c_pct_cpu_t) 
	_prevCpuUsage = *cur_c_cpu_t
	prevSampleTs = util.NowEpoch()
		
	return &CpuPctUsageInfo{ 
		User : float64(c_pct_cpu_t.user), 
		Sys : float64(c_pct_cpu_t.sys), 
		Nice : float64(c_pct_cpu_t.nice), 
		Idle : float64(c_pct_cpu_t.idle), 
		Wait : float64(c_pct_cpu_t.wait), 
		Irq : float64(c_pct_cpu_t.irq), 
		SoftIrq : float64(c_pct_cpu_t.soft_irq), 
		Stolen : float64(c_pct_cpu_t.stolen), 
		Combined : float64(c_pct_cpu_t.combined), 
	},nil 

}

//------------------------------------------------------------------------------------------------------------------------------------------------
//CpuUsageInfo (uint64) CpuUsageInfo (uint64) CpuUsageInfo (uint64) CpuUsageInfo (uint64) CpuUsageInfo (uint64) CpuUsageInfo (uint64) 
//------------------------------------------------------------------------------------------------------------------------------------------------
func GetCpuUsageInfo() (result *CpuUsageInfo, err error) { 
	defer util.Panic2Error(&err)
	
	c_cpu_t, err := _getCpu(GetSigarHandle()) 
	if err != nil { 
		return nil,err 
	}
	
	return &CpuUsageInfo{ 
		User : uint64(c_cpu_t.user), 
		Sys : uint64(c_cpu_t.sys), 
		Nice : uint64(c_cpu_t.nice), 
		Idle : uint64(c_cpu_t.idle), 
		Wait : uint64(c_cpu_t.wait), 
		Irq : uint64(c_cpu_t.irq), 
		SoftIrq : uint64(c_cpu_t.soft_irq), 
		Stolen : uint64(c_cpu_t.stolen), 
		Total : uint64(c_cpu_t.total), 
	},nil
}

//------------------------------------------------------------------------------------------------------------------------------------------------
//Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers 
//------------------------------------------------------------------------------------------------------------------------------------------------
func _getCpu(sigar *C.sigar_t) (*C.sigar_cpu_t, error) { 

	var c_cpu_t C.sigar_cpu_t
	if status := int(C.sigar_cpu_get(sigar, &c_cpu_t)) ; status != SIGAR_OK { 
		return nil,fmt.Errorf("Failed to retrieve cpu usage info with error: %v", status)
	}
	
	return &c_cpu_t,nil
}
//------------------------------------------------------------------------------------------------------------------------------------------------
//List List List List List List List List List List List List List List List List List List List List List List List List List List List List 
//------------------------------------------------------------------------------------------------------------------------------------------------

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



 



