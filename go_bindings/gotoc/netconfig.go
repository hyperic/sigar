package gotoc


import (
		
	"unsafe"
	
	
)
/*

#include "../../../../../Include/sigar.h"
#include<stdlib.h>
#include <stdio.h>
typedef unsigned char byte;
	void translateNetConfig(sigar_t *sigar){
	
	
	
		
		sigar_net_interface_config_t config;

		

   
		 sigar_net_interface_config_get(sigar, "eth10", &config);
		 sigar_uint32_t a=config.address.addr.in; 
           
		 
		                  char buf[180];
                  unsigned char* ucp = (unsigned char *)&a;
                  sprintf(buf, "%d.%d.%d.%d", ucp[0] & 0xff, ucp[1] & 0xff, ucp[2] & 0xff, ucp[3] & 0xff);
                  printf("%s\n",buf);

		

 		

	
	}


*/
import "C"


func GetNetConfig()(C.sigar_net_interface_config_t,[]C.sigar_net_interface_config_t,[]C.sigar_net_interface_stat_t){

	var sigar *C.sigar_t=GetSigarHandle()
	var netlistconf C.sigar_net_interface_list_t
	var netconfPrimary C.sigar_net_interface_config_t
	
	C.sigar_net_interface_config_primary_get(sigar, &netconfPrimary)
	
	C.sigar_net_interface_list_get(sigar, &netlistconf)
	
	var length int=int(netlistconf.number)
	
	goInterfacesNames := ((*[1 << 30]*C.char)(unsafe.Pointer(netlistconf.data)))[:length]	
	 
	//cinteraces:=GetGoSlice(length, unsafe.Pointer(netlistconf.data))
	//goInterfacesNames := *(*[]C.char)(unsafe.Pointer(&cinteraces))
	
	var netconf =make([]C.sigar_net_interface_config_t,length)
	var netstat =make([]C.sigar_net_interface_stat_t,length)
	
	for i:=0;i<length;i++{

		C.sigar_net_interface_config_get(sigar, goInterfacesNames[i],
		&netconf[i])
		
		C.sigar_net_interface_stat_get(sigar, goInterfacesNames[i],
		&netstat[i])
	}
	
	C.sigar_net_interface_list_destroy(sigar, &netlistconf)
	
	return netconfPrimary, netconf, netstat
}

func GetNetInterfaceConfig(){
	
	
	//_,netconf,_:=GetNetConfig()
	
	
	var sigar *C.sigar_t=GetSigarHandle()
 	C.translateNetConfig(sigar)
	
	
	
}

type NetworkService struct {
	name string
	RawIdentifier string
	Id string
	Broadcast string
	Address string
	NetMask string
	Flags string
	Mtu string
	Mac string
	
	
}

