package gotoc

import (
	"fmt"
	"time"
	"strconv"
	"github.com/vmware/leap/util"
)
/*
#include "../../../../../Include/sigar.h"
*/
import "C"

type UptimeInfo struct { 
	Time  time.Time
	Uptime float64
	LoadAvg []float64
} 
func (this *UptimeInfo) String() string { 
	
	_uptime :=  this.Time.Format(time.Kitchen)  + " up"  
	days := int((this.Uptime / (60 * 60 * 24)))
	var minutes, hours int 

	if days != 0 {
		 _uptime = _uptime + " " + strconv.Itoa(days) + " " 
		 if days > 1 { 
		 	_uptime = _uptime + "days" 
		 }else{ 
		 	_uptime = _uptime + "day" 
		}
		_uptime = _uptime + ", "
     }

	minutes = int(this.Uptime / 60)
	hours = int(minutes / 60)
    hours = hours % 24
    minutes = minutes % 60

	if hours != 0 {
		_uptime = _uptime + strconv.Itoa(hours) + ":" 
		if minutes < 10 { 
			_uptime = _uptime + "0" 
		}
		_uptime = _uptime + strconv.Itoa(minutes);
    }else{
		_uptime = _uptime + strconv.Itoa(minutes) + " min";
	}
    
    for _,d := range this.LoadAvg { 
    	_uptime = _uptime + ", " + strconv.FormatFloat(d, 'f', 2, 64) 
    }
    
    return _uptime 
}


func GetUptimeInfo() (result *UptimeInfo, err error) { 
	defer util.Panic2Error(&err)
	var sigar *C.sigar_t=GetSigarHandle()
	
	uptime, err := _uptime(sigar) 
	if err != nil { 
		return nil,err 
	}
	
	loadAvg, err := _loadAvg(sigar) 
	if err != nil { 
		return nil,err 
	}
	
	return &UptimeInfo{ Time: time.Now(),  Uptime : uptime, LoadAvg : loadAvg},nil 
}

func _uptime(sigar *C.sigar_t) (result float64, err error){

	var uptime C.sigar_uptime_t
	
	if status := int(C.sigar_uptime_get(sigar, &uptime)); status != SIGAR_OK { 
		return -1,fmt.Errorf("Failed to rertieve uptime with error: %v", status)
	}
	return float64(uptime.uptime),nil 

}

func _loadAvg(sigar *C.sigar_t) (result []float64, err error){
	var c_loadAvg C.sigar_loadavg_t
	
	if status :=  int(C.sigar_loadavg_get(sigar, &c_loadAvg)) ; status != SIGAR_OK { 
		return nil,fmt.Errorf("Failed to rertieve load avg with error: %v", status)
	}
	
	loadAvg := (([3]C.double)(c_loadAvg.loadavg)) //[:length]	

	return []float64{ float64(loadAvg[0]), float64(loadAvg[1]), float64(loadAvg[2]) },nil
	
}