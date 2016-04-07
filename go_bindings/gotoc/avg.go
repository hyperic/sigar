package gotoc

import (
	
	
	
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"

func Avg()C.sigar_loadavg_t{

	var sigar *C.sigar_t=GetSigarHandle()
	
	var avg C.sigar_loadavg_t
	
	
	C.sigar_loadavg_get(sigar, &avg)
	
	
	return avg

}