package gotoc


import (
	

	"unsafe"
	


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

