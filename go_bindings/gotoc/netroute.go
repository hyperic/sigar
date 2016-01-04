package gotoc


import (
	

	"unsafe"
	


)
/*
#include "../../../../../Include/sigar.h"

       
        
*/  
import "C"

   

func NetRoute() (C.sigar_net_route_list_t ,
[]C.sigar_net_route_t){

      
   	sigar:=GetSigarHandle()
	
	var netlist C.sigar_net_route_list_t 
	
	
	
	C.sigar_net_route_list_get(sigar, &netlist)
	var length int=int(netlist.number)
	
	var goNetRoute =make([]C.sigar_net_route_t, length) 
    
    
    
	
	croute:=GetGoSlice(length, unsafe.Pointer(netlist.data))
	
	goNetRoute = *(*[]C.sigar_net_route_t)(unsafe.Pointer(&croute))	
	
	
	C.sigar_net_route_list_destroy(sigar, &netlist)
		
		
	return netlist, goNetRoute
	
	
	 
	
    
    
 
}

