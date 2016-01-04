package gotoc


import (
	

	
)
/*

#include "../../../../../Include/sigar.h"

 

*/
import "C"

func NetInfo() C.sigar_net_info_t{

	var sigar *C.sigar_t=GetSigarHandle()
	var netInfo C.sigar_net_info_t
	C.sigar_net_info_get(sigar, &netInfo)
	
	
	
	return netInfo

}