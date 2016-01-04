package gotoc


import (
	"fmt"
	"github.com/vmware/leap/util"
)
/*
#include "../../../../../Include/sigar.h"
*/
import "C"

type NetInfo struct { 
	DefaultGateway string
	DefaultGatewayInterface string
	HostName string 
	DomainName string 
	PrimaryDns string 
	SecondaryDns string
	Fqdn string
}
func (this *NetInfo) String() string { return "{ DefaultGateway:" + this.DefaultGateway + ", " + 
	"DefaultGatewayInterface: " + this.DefaultGatewayInterface + ", " + 
	"HostName: " + this.HostName + ", " + 
	"DomainName: " + this.DomainName + ", " + 
	"PrimaryDns: " + this.PrimaryDns + ", " + 
	"SecondaryDns: " + this.SecondaryDns + ", " + 
	"Fqdn: " + this.Fqdn + "}"
}	

func GetNetInfo() (netInfo *NetInfo, err error){
	defer util.Panic2Error(&err)
	
	sigar := GetSigarHandle()
	var c_netInfo C.sigar_net_info_t
	C.sigar_net_info_get(sigar, &c_netInfo)
	
	//[512] 
	var c_fqdn [C.SIGAR_FQDN_LEN]C.char
	if status := C.sigar_fqdn_get(sigar, &c_fqdn[0], C.SIGAR_FQDN_LEN) ; status != SIGAR_OK { 
		return nil,fmt.Errorf("Failed to retrieve fqdn due to " + err.Error())
	}


	return &NetInfo{ 
		DefaultGateway: C.GoString(&c_netInfo.default_gateway[0]), 
		DefaultGatewayInterface: C.GoString(&c_netInfo.default_gateway_interface[0]), 
		HostName: C.GoString(&c_netInfo.host_name[0]), 
		DomainName: C.GoString(&c_netInfo.domain_name[0]), 
		PrimaryDns: C.GoString(&c_netInfo.primary_dns[0]), 
		SecondaryDns: C.GoString(&c_netInfo.secondary_dns[0]), 
		Fqdn : C.GoString(&c_fqdn[0]), 
	},nil

}