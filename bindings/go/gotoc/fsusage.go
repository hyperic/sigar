package gotoc


import (
	"fmt"
	"unsafe"
)
/*
#include <stdlib.h>
#include "../../../../../Include/sigar.h"
*/
import "C"

type sigar_file_system_usage_t struct { 	
	UsedPct float64
	Total int64
	Free  int64
	Used  int64
	Avail int64
	Files int64
	FreeFiles  int64
}

type FsUsage struct { 	
	UsedPct float64
	Total int64
	Free  int64
	Used  int64
	Avail int64
	Files int64
	FreeFiles  int64
}

func (this *FsUsage) String() string { return fmt.Sprintf("used: %v, total: %v, free: %v, used: %v, avail: %v, files: %v, freeFiles: %v", this.UsedPct, this.Total, this.Free, this.Used, this.Avail, this.Files, this.FreeFiles) }

type DiskUsage struct { 
	Reads int64
	Writes int64
	WriteBytes int64
	ReadBytes int64
	Rtime int64
	Wtime  int64
	Qtime  int64
	Time  int64
	SnapTime  int64
	ServiceTime float64 
	Queue float64
}

type FsUsageInfo struct { 
	Fs *FsUsage
	Disk *DiskUsage
}

func GetFsUsageInfo(dirName string, diskName string)(fsUsageInfo *FsUsageInfo,err error){
	
	var sigar *C.sigar_t=GetSigarHandle()
	//TODO: noticed that sigar_file_system_usage_t contains a sigar_disk_usage_t member, should there be two invocations?
	fsUsage, err := _getFsUsage(dirName, sigar) 
	if err != nil { 
		return nil, err
	}
	
	diskUsage, err := _getDiskUsage(diskName, sigar) ; 
	if err != nil { 
		return nil,err
	}
	

	return &FsUsageInfo {fsUsage, diskUsage}, err
}


func GetFsUsage(dirName string) (*FsUsage, error) { 
	return _getFsUsage(dirName, GetSigarHandle()) 
}

func _getFsUsage(dirName string, sigar *C.sigar_t)  (out *FsUsage, err error) { 
	var fsusage C.sigar_file_system_usage_t
	
	dir:=C.CString(dirName)
	
	C.free(unsafe.Pointer(dir))
	
	C.sigar_file_system_usage_get(sigar , dir, &fsusage)
	
	out = &FsUsage{ 
		float64(fsusage.use_percent),
		int64(fsusage.total),
		int64(fsusage.free),
		int64(fsusage.used),
		int64(fsusage.avail),
		int64(fsusage.files),
		int64(fsusage.free_files),
	}
	
	return out,nil
}

func GetDiskUsage(diskName string) (*DiskUsage, error) { 
	return _getDiskUsage(diskName, GetSigarHandle()) 
}

func _getDiskUsage(diskName string, sigar *C.sigar_t)  (*DiskUsage, error) { 
	var diskusage C.sigar_disk_usage_t
	disk:=C.CString(diskName)
	defer C.free(unsafe.Pointer(disk))
	C.sigar_disk_usage_get(sigar , disk, &diskusage)
	return &DiskUsage { 
		int64(diskusage.reads), 
		int64(diskusage.writes), 
		int64(diskusage.write_bytes), 
		int64(diskusage.read_bytes), 
		int64(diskusage.rtime), 
		int64(diskusage.wtime), 
		int64(diskusage.qtime), 
		int64(diskusage.time), 
		int64(diskusage.snaptime), 
		float64(diskusage.service_time), 
		float64(diskusage.queue), 
	},nil
}