package gotoc

import (
	"unsafe"
	"github.com/vmware/leap/util"
)

/*
#include "../../../../../Include/sigar.h"
#cgo CFLAGS:  -I../../../../../Include
#cgo darwin LDFLAGS: -L ${SRCDIR}/../../../../../Libs/darwin/  -lsigar         
#cgo linux LDFLAGS: -ldl  -L ${SRCDIR}/../../../../../Libs/linux-amd64/ -lsigar
*/  
import "C" 
 
var SYS_INFO *SysInfo

func init() { 
	
	var err error 
	if SYS_INFO,err = GetSysInfo() ; err != nil { 
		panic("Failed to extract system info due to " + err.Error()) 
	}
}

type SysInfo struct {
	Name string `json:"type"`
	Version string `json:"version"`
	Arch string `json:"arch"`
	Machine string `json:"-"`
	Description string `json:"description"`
	PatchLevel string `json:"patchLevel"`
	Vendor string `json:"vendor"`
	VendorVersion string `json:"version"`
	VendorName string `json:"vendorName"`
	VendorCodeName string `json:"codeName"`
}

func GetSysInfo() (sysinfo *SysInfo, err error) { 
	defer util.Panic2Error(&err) 
	
	var sigar *C.sigar_t=GetSigarHandle()
	var c_sysInfo C.sigar_sys_info_t
	C.sigar_sys_info_get(sigar, &c_sysInfo)
	
	return &SysInfo{ 
		Name : C.GoString(&c_sysInfo.name[0]), 
		Version : C.GoString(&c_sysInfo.version[0]),
		Arch : C.GoString(&c_sysInfo.arch[0]),  
		Machine : C.GoString(&c_sysInfo.machine[0]),
		Description : C.GoString(&c_sysInfo.description[0]),
		PatchLevel : C.GoString(&c_sysInfo.patch_level[0]),
		Vendor : C.GoString(&c_sysInfo.vendor[0]), 
 		VendorName : C.GoString(&c_sysInfo.vendor_name[0]), 
		VendorVersion : C.GoString(&c_sysInfo.vendor_version[0]), 
		VendorCodeName : C.GoString(&c_sysInfo.vendor_code_name[0]),    
	},nil
}

func _GetSysInfo() (C.sigar_sys_info_t, []C.sigar_who_t) {
	
	var sigar *C.sigar_t=GetSigarHandle()
	
	var sysInfo C.sigar_sys_info_t
    
    var wholist C.sigar_who_list_t 
    
    
    C.sigar_sys_info_get(sigar, &sysInfo)
    
    C.sigar_who_list_get(sigar,&wholist)
    
    var length int=int(wholist.number)
	
	var goWho =make([]C.sigar_who_t, length) 
    
    
    
	
	cwho:=GetGoSlice(length, unsafe.Pointer(wholist.data))
	
	goWho = *(*[]C.sigar_who_t)(unsafe.Pointer(&cwho))	
	
       
    C.sigar_who_list_destroy(sigar , &wholist)
      
	
	return sysInfo, goWho
	
}



