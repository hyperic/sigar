package gotoc

import (
	
	
	
)
/*
#include "../../../../../Include/sigar.h"



*/
import "C"

func Resources()C.sigar_resource_limit_t{
	
	var resources C.sigar_resource_limit_t
	
	C.sigar_resource_limit_get(sigar, &resources)
	
	
	return resources

}