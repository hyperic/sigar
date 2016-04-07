package gotoc


import (
	"unsafe"
	"fmt"
	"strconv"
	"github.com/vmware/leap/util"
	log "github.com/jbrodriguez/mlog"
)
/*
#include "../../../../../Include/sigar.h"
*/  
import "C"

   

func Ps() ([]C.sigar_proc_state_t ,
[]C.sigar_proc_time_t,
[]C.sigar_proc_mem_t, 
[]C.sigar_proc_disk_io_t,
[]C.sigar_proc_cumulative_disk_io_t,
[]C.sigar_proc_cred_t,
[]C.sigar_proc_cred_name_t,
[]C.sigar_proc_cpu_t,
[]C.sigar_proc_args_t,
[]C.sigar_proc_fd_t,
[]C.sigar_proc_exe_t){

      
   	sigar:=GetSigarHandle()
	
	var procList C.sigar_proc_list_t 
	
	
	
	C.sigar_proc_list_get(sigar, &procList)
	var length int=int(procList.number)
	
	var pstate =make([]C.sigar_proc_state_t, length) 
    var ptime =make([]C.sigar_proc_time_t,length)
    var pmem =make([]C.sigar_proc_mem_t,length)
    var pdiskio =make([]C.sigar_proc_disk_io_t,length)
    var pcumdiskio =make([]C.sigar_proc_cumulative_disk_io_t,length)
    var pcred =make([]C.sigar_proc_cred_t,length)
    var pcredname =make([]C.sigar_proc_cred_name_t,length)
    var pcpu =make([]C.sigar_proc_cpu_t,length)
    var pargs =make([]C.sigar_proc_args_t,length)
    var pfd =make([]C.sigar_proc_fd_t,length)
    var pexe =make([]C.sigar_proc_exe_t,length)
     
    
	
	cpid:=GetGoSlice(length, unsafe.Pointer(procList.data))
	var goPid []C.sigar_pid_t
	goPid = *(*[]C.sigar_pid_t)(unsafe.Pointer(&cpid))	
	
	for i:=0;i<length;i++{
	
		
		C.sigar_proc_state_get(sigar, goPid[i], &pstate[i])
		C.sigar_proc_time_get(sigar, goPid[i], &ptime[i])
		C.sigar_proc_mem_get(sigar, goPid[i], &pmem[i])
		C.sigar_proc_disk_io_get(sigar, goPid[i], &pdiskio[i])
		C.sigar_proc_cumulative_disk_io_get(sigar, goPid[i], &pcumdiskio[i])
		C.sigar_proc_cred_get(sigar, goPid[i], &pcred[i])
		C.sigar_proc_cred_name_get(sigar, goPid[i], &pcredname[i])
		C.sigar_proc_cpu_get(sigar, goPid[i], &pcpu[i])	
		C.sigar_proc_fd_get(sigar, goPid[i], &pfd[i])
		C.sigar_proc_exe_get(sigar, goPid[i], &pexe[i])
		
		
		C.sigar_proc_args_get(sigar, goPid[i], &pargs[i])
		C.sigar_proc_args_destroy(sigar, &pargs[i])
		
		
		
	
	}
	
	 
	
	
	C.sigar_proc_list_destroy(sigar, &procList)

	return pstate, 
	ptime, 
	pmem, 
	pdiskio, 
	pcumdiskio, 
	pcred, 
	pcredname,
	pcpu,
	pargs, 
	pfd,
	pexe
	
 
}
//-------------------------------------------------------------------------------
//ProcessInfoList ProcessInfoList ProcessInfoList ProcessInfoList ProcessInfoList 
//-------------------------------------------------------------------------------
func GetProcInfo(pid uint64, infoTypesMask ProcInfoType) (procInfo *ProcessInfo, err error) { 
	procInfo,_,err = _getProcInfo(pid, infoTypesMask, nil, GetSigarHandle())
	return
}

func GetAllProcInfos(infoTypesMask ProcInfoType) (processes *ProcessInfoList, err error) { 	
	
	var procList C.sigar_proc_list_t 
	defer util.Panic2Error(&err)
	defer C.sigar_proc_list_destroy(sigar, &procList) 
	
	sigar:=GetSigarHandle()
	
	C.sigar_proc_list_get(sigar, &procList)
	
	return getProcInfos(&procList, infoTypesMask, sigar)
}

func getProcInfos(procList *C.sigar_proc_list_t, infoTypesMask ProcInfoType, sigar *C.sigar_t) (processes *ProcessInfoList, err error) { 	
	defer util.Panic2Error(&err)
	
	noOfProcesses := int(procList.number)
	pids :=  *(*[]C.sigar_pid_t) (CArr2SlicePtr(noOfProcesses, procList.data))	

	procInfos := make([]*ProcessInfo, noOfProcesses)
	var errors, index int
	var sysMemInfoOut *MemInfo
	for _,pid := range pids { 
		
		procInfo,sysMemInfo, err := _getProcInfo(uint64(pid), infoTypesMask, sysMemInfoOut, sigar) 
		
		if err != nil { 
			log.ErrorS(err.Error()) 
			errors = errors+1
			continue 
		}
		
		if sysMemInfoOut == nil { 
			sysMemInfoOut = sysMemInfo
		}
		
		procInfos[index] = procInfo
		index = index+1
	}
	
	if errors > 0 { 
		newSize := noOfProcesses-errors
		procInfos = append(([]*ProcessInfo)(nil), procInfos[:newSize]...)
		log.ErrorS("%v errors have occurred, prunning array size from %v to %v index is: %v, newSize: %v", errors, noOfProcesses, newSize, index, len(procInfos)) 
	}
	
	return &ProcessInfoList { procInfos, sysMemInfoOut },nil
}

//---------------------------------------------------------------------------
//ProcInfo ProcInfo ProcInfo ProcInfo ProcInfo ProcInfo ProcInfo ProcInfo 
//---------------------------------------------------------------------------

type ProcInfoType uint64
const ( 
	MEM ProcInfoType = 1 << iota
	CPU
	TIME
	DISK
	ARGS
	STATE
	CRED_NAME
) 

type ProcessInfoList struct { 
	Processes []*ProcessInfo
	SysMemInfo *MemInfo
}

type ProcessInfo struct { 
	Pid uint64
	Args []string
	State *ProcessStateInfo
	CredName *ProcessCredName
	Mem *ProcessMemInfo
	Cpu *ProcessCpuInfo
	Disk *ProcessDiskInfo
	Error error
}

type ProcessCredName struct { 
	User string
	Group string
}

type ProcessStateInfo struct { 
	Name string 
	State rune 
	Tty	int 
	Priority int 
	Nice int 
	Processor int 
	Threads uint64
}

type ProcessMemInfo struct { 
	Size uint64
	Resident uint64
	Share uint64
	MinorFaults uint64
	MajorFaults uint64
	PageFaults uint64
	Percent float64
}

type ProcessCpuInfo struct { 
	StartTime uint64
	User uint64
	Sys uint64
	Total uint64
	LastTime uint64
	Percent float64
}

type ProcessDiskInfo struct{ //sigar_proc_cumulative_disk_io_t
	BytesRead uint64
	BytesWritten uint64
	BytesTotal uint64
}


func _getProcInfo(pid uint64, infoTypesMask ProcInfoType, sysMemInfoIn *MemInfo, sigar *C.sigar_t) (procInfo *ProcessInfo, sysMemInfo *MemInfo, err error) { 
	
	procInfo = &ProcessInfo{ Pid : pid }
	
	sigarPid := C.sigar_pid_t(pid) 
		
	if infoTypesMask&MEM != 0 { 
		sysMemInfo = sysMemInfoIn 
		if sysMemInfo == nil { 
			sysMemInfo,err = GetMemInfo() 
			if err != nil { 
				return nil,nil,err 
			}
		}	
		if err = _populateProcessMemInfo(sigarPid, procInfo, sysMemInfo, sigar) ; err != nil { 
			return nil,nil,err 
		}
	}
	
	if infoTypesMask&CPU != 0 { 	
		if err = _populateProcessCpuInfo(sigarPid, procInfo, sigar) ; err != nil { 
			return nil,nil,err 
		}
	}
	
	if infoTypesMask&TIME != 0 {  	
	}
	
	if infoTypesMask&DISK != 0 { 	
		if err = _populateProcessDiskInfo(sigarPid, procInfo, sigar) ; err != nil { 
			return nil,nil,err 
		}
	}
	
	if infoTypesMask&ARGS != 0 { 	
		if err = _populateProcessArgs(sigarPid, procInfo, sigar) ; err != nil { 
			return nil,nil,err 
		}
	}
	
	if infoTypesMask&STATE != 0 { 	
		if err = _populateProcessState(sigarPid, procInfo, sigar) ; err != nil { 
			return nil,nil,err 
		}
	}
	
	if infoTypesMask&CRED_NAME != 0 { 	
		if err = _populateProcessCredName(sigarPid, procInfo, sigar) ; err != nil { 
			return nil,nil,err 
		}
	}
	
	
	return procInfo,sysMemInfo,nil

}

func _populateProcessMemInfo(pid C.sigar_pid_t, procInfo *ProcessInfo, sysMemInfo *MemInfo, sigar *C.sigar_t) error { 
	
	var procMem C.sigar_proc_mem_t 
	status := int(C.sigar_proc_mem_get(sigar, pid, &procMem)) 
	if status != SIGAR_OK { 
		return fmt.Errorf("Failed to rertieve proc mem info for pid: "+ strconv.FormatUint(procInfo.Pid,10) +" with error: " + strconv.Itoa(status))
	}
	
	procInfo.Mem = &ProcessMemInfo{ 
		Size : uint64(procMem.size), 
		Resident : uint64(procMem.resident), 
		Share : uint64(procMem.share), 
		MinorFaults : uint64(procMem.minor_faults), 
		MajorFaults : uint64(procMem.major_faults), 
		PageFaults : uint64(procMem.page_faults), 
	}
	
	procInfo.Mem.Percent = float64(sysMemInfo.Mem.Total/procInfo.Mem.Resident)
	
	return nil 
}

func _populateProcessCpuInfo(pid C.sigar_pid_t, procInfo *ProcessInfo, sigar *C.sigar_t) error { 
	
	var procCpu C.sigar_proc_cpu_t 
	status := int(C.sigar_proc_cpu_get(sigar, pid, &procCpu)) 
	if status != SIGAR_OK { 
		return fmt.Errorf("Failed to rertieve proc mem info for pid: %v", procInfo.Pid)
	}
	
	procInfo.Cpu = &ProcessCpuInfo{ 
		StartTime : uint64(procCpu.start_time), 
		User : uint64(procCpu.user),
		Sys : uint64(procCpu.sys),
		Total : uint64(procCpu.total),
		LastTime : uint64(procCpu.last_time),
		Percent : float64(procCpu.percent),
	}
	
	return nil 
}

func _populateProcessDiskInfo(pid C.sigar_pid_t, procInfo *ProcessInfo, sigar *C.sigar_t) error { 
	
	var c_procDiskIo C.sigar_proc_disk_io_t  
	status := int(C.sigar_proc_disk_io_get(sigar, pid, &c_procDiskIo)) 
	if status != SIGAR_OK { 
		//TODO: restore error (unsupported on mac)
		//return fmt.Errorf("Failed to rertieve proc disk io info for pid: %v with error: %v", procInfo.Pid, status)
		procInfo.Disk = &ProcessDiskInfo{ 
			BytesRead : 100000,
			BytesWritten : 23423432,
			BytesTotal : 2345543,
		}
	}
	
	procInfo.Disk = &ProcessDiskInfo{ 
		BytesRead : uint64(c_procDiskIo.bytes_read),
		BytesWritten : uint64(c_procDiskIo.bytes_written),
		BytesTotal : uint64(c_procDiskIo.bytes_total),
	}
	
	return nil 
}

func _populateProcessArgs(pid C.sigar_pid_t, procInfo *ProcessInfo, sigar *C.sigar_t) error { 
	
	var c_procArgs C.sigar_proc_args_t 
	defer C.sigar_proc_args_destroy(sigar, &c_procArgs)
	
	status := int(C.sigar_proc_args_get(sigar, pid, &c_procArgs)) 
	if status != SIGAR_OK { 
		return fmt.Errorf("error: proc args for pid: " + strconv.FormatUint(procInfo.Pid ,10))
	}
	
	noOfArgs := int(c_procArgs.number)
	args :=  *(*[]*C.char) (CArr2SlicePtr(noOfArgs, c_procArgs.data))	

	procInfo.Args = make([]string, noOfArgs)
	for i,arg := range args { 
		procInfo.Args[i] = C.GoString(arg) 	
	}
	
	return nil 
}

func _populateProcessState(pid C.sigar_pid_t, procInfo *ProcessInfo, sigar *C.sigar_t) error { 
		
	var c_procState C.sigar_proc_state_t 
	status := int(C.sigar_proc_state_get(sigar, pid, &c_procState)) 
	if status != SIGAR_OK { 
		return fmt.Errorf("Failed to rertieve proc state info for pid: %v", procInfo.Pid)
	}
	
	procInfo.State = &ProcessStateInfo{ 
		Name : C.GoString(&c_procState.name[0]), 
		State: rune(c_procState.state), 
		Tty : int(c_procState.tty),
		Priority : int(c_procState.priority),
		Nice : int(c_procState.nice),
		Processor : int(c_procState.processor), 
		Threads : uint64(c_procState.threads),
	}
	
	return nil 
}

var c_credName C.sigar_proc_cred_name_t 
func _populateProcessCredName(pid C.sigar_pid_t, procInfo *ProcessInfo, sigar *C.sigar_t) error { 
		
	status := int(C.sigar_proc_cred_name_get(sigar, pid, &c_credName)) 
	if status != SIGAR_OK { 
		return fmt.Errorf("Failed to rertieve proc cred name for pid: %v", procInfo.Pid)
	}
	
	procInfo.CredName = &ProcessCredName{ 
		User : C.GoString(&c_credName.user[0]), 
		Group : C.GoString(&c_credName.group[0]), 
	}
	
	return nil 
}

