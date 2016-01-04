package gotoc

import (
	
	
	
)
/*

#include "../../../../../Include/sigar.h"


*/
import "C"

func UpTime()C.sigar_uptime_t{

	var sigar *C.sigar_t=GetSigarHandle()
	
	var uptime C.sigar_uptime_t
	
	C.sigar_uptime_get(sigar, &uptime)
	
	
	return uptime

}