package gotoc


import (
	"fmt"
	"unsafe"
	"strconv"
)
/*
#include "../../../../../Include/sigar.h"



*/
import "C"


const ( 
	FSTYPE_UNKNOWN = iota
    FSTYPE_NONE
    FSTYPE_LOCAL_DISK
    FSTYPE_NETWORK
    FSTYPE_RAM_DISK
    FSTYPE_CDROM
    FSTYPE_SWAP
    FSTYPE_MAX
)


type FsInfo struct { 
	DirName string 
	DevName string 
	TypeName string
	SysTypeName string  
	Options string 
	Type int 
	Flags uint64
}
func (this *FsInfo) String() string {
	 return fmt.Sprintf("{Dirname: %v, DevName: %v, TypeName: %v, SysTypeName: %v, Options: %v, Type: %v, Flags:%v", 
		this.DirName, 
		this.DevName, 
		this.TypeName, 
		this.SysTypeName, 
		this.Options, 
		this.Type,
		this.Flags,
	)
}	 

func GetFsInfo() (result []*FsInfo, err error){
	defer func() { 
		if r := recover() ; r != nil{
			err = fmt.Errorf("Failed to discover file systems due to: " + err.Error())
		}
	}() 
	var sigar *C.sigar_t=GetSigarHandle()
	var fileSystemList C.sigar_file_system_list_t 
	C.sigar_file_system_list_get(sigar, &fileSystemList);
	defer C.sigar_file_system_list_destroy(sigar, &fileSystemList);
	
	var length int=int(fileSystemList.number)
	
	cFs:=GetGoSlice(length, unsafe.Pointer(fileSystemList.data))
	
	var goFs []C.sigar_file_system_t
	goFs = *(*[]C.sigar_file_system_t)(unsafe.Pointer(&cFs))	
	
	result = make([]*FsInfo, length)  
	for i,fs := range goFs { 
		fsType,err := strconv.Atoi(fmt.Sprintf("%v", fs._type))
		if err != nil { 
			fsType = FSTYPE_UNKNOWN
		}
		result[i] = &FsInfo{
			DirName : C.GoString(&fs.dir_name[0]), 
			DevName : C.GoString(&fs.dev_name[0]), 
			TypeName : C.GoString(&fs.type_name[0]), 
			SysTypeName : C.GoString(&fs.sys_type_name[0]), 
			Options : C.GoString(&fs.options[0]), 
			Type : fsType, 
			Flags : uint64(fs.flags), 
		}
	}	 
	
	return result,nil
	
}
   



