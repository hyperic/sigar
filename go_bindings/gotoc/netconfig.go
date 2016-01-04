package gotoc


import (
	"fmt"	
	"unsafe"
	"github.com/vmware/leap/util"
)
/*

#include "../../../../../Include/sigar.h"
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;
 void translateNetConfig(sigar_t *sigar){
 
        sigar_net_interface_list_t net_iflist;
        size_t i;
       
        sigar_net_interface_list_get(sigar, &net_iflist);
        printf("number: %lu\n", net_iflist.number) ; 
        for (i = 0; i < net_iflist.number; i++) {
              char *ifname = net_iflist.data[i];
              sigar_net_interface_stat_t  ifstat;
              sigar_net_interface_config_t config;
             
              int ret=sigar_net_interface_config_get(sigar, ifname, &(config));
              if(ret!=SIGAR_OK){
                    printf("error number: %d\n",ret);
                    return;
              }
              sigar_uint32_t a=config.address.addr.in;
              char buf[180];
              unsigned char* ucp = (unsigned char *)&a;
              sprintf(buf, "%d.%d.%d.%d", ucp[0] & 0xff, ucp[1] & 0xff, ucp[2] & 0xff, ucp[3] & 0xff);
               printf("%s: %s\n",ifname, buf);
       
       
        }
        sigar_net_interface_config_t netconfPrimary;
        sigar_net_interface_config_primary_get(sigar, &netconfPrimary);
        sigar_uint32_t a=netconfPrimary.address.addr.in;
             
              char buf[18];
              unsigned char* ucp = (unsigned char *)&a;
              sprintf(buf, "%d.%d.%d.%d", ucp[0] & 0xff, ucp[1] & 0xff, ucp[2] & 0xff, ucp[3] & 0xff);
              printf("%s\n",buf);
       
        sigar_net_interface_list_destroy(sigar, &net_iflist);
       
       
        //char *name=netinfo->name;
        //char *type=netinfo->type;
       
        //sigar_uint32_t a=netinfo->address.addr.in;
        //printf("%s broadcast:\n", netinfo->address.addr.mac[0]);
        //printf("%d.%d.%d.%d", (byte)a>>24 & 0xFF, (byte)a>>16 & 0xFF, (byte)a>>8& 0xFF, (byte)a& 0xFF);
  }
                

*/
import "C"

type NetConfig struct { 
}

type NetAddress struct{ 
	Family string 
	In uint32 
	In6 uint32
	Mac string 
}
func (this *NetAddress) String() string { return fmt.Sprintf("{ Family: %v, In: %v, in6: %v, Mac: %v }", this.Family, this.In, this.In6, this.Mac) }

type InterfaceConfigInfo struct { 
	Name string
	Type string 
	Description string
	Hwaddr *NetAddress
	Address *NetAddress
	Destination *NetAddress
	Broadcast *NetAddress
	Netmask *NetAddress
	Address6 *NetAddress
    Prefix6Length int
    Scope6 int 
    Flags uint64
    Mtu uint64
    Metric uint64
    TxQueueLen int 
}
func (this *InterfaceConfigInfo) String() string { 
	return fmt.Sprintf(`Name: %v, Type: %v, Description: %v, Hwaddr: %v, Address: %v, Destination: %v, Broadcast: %v, Netmask: %v, Address6: %v, Prefix6Length: %v, Scope6: %v, Flags: %v, Mtu: %v, Metric: %v, TxQueueLen: %v`, 
		this.Name, 
		this.Type, 
		this.Description, 
		this.Hwaddr, 
		this.Address,
		this.Destination, 
		this.Broadcast, 
		this.Netmask,
		this.Address6, 
		this.Prefix6Length,
		this.Scope6, 
		this.Flags, 
		this.Mtu,
		this.Metric,
		this.TxQueueLen,	
	)
}

type NetIfaceUsageInfo struct { 
	RxPackets uint64
    RxBytes uint64
    RxErrors uint64
    RxDropped uint64
    RxOverruns uint64
    RxFrame uint64
    /* transmitted */
    TxPackets uint64
    TxBytes uint64
    TxErrors uint64
    TxDropped uint64
    TxOverruns uint64
    TxCollisions uint64
    TxCarrier uint64
    Speed uint64
}

//----------------------------------------------------------------------------------------
//Top-Level Top-Level Top-Level Top-Level Top-Level Top-Level Top-Level Top-Level Top-Level 
//----------------------------------------------------------------------------------------
func GetNetInterfaceConfig() (result []*InterfaceConfigInfo, err error) { 
	return getNetInterfaceConfigWithSigar(GetSigarHandle()), nil
}

func GetNetIfaceUsageInfo(netIfaceName string) (netifaceInfo *NetIfaceUsageInfo, err error) { 
	defer util.Panic2Error(&err)
	
	c_netifaceName := C.CString(netIfaceName) 
	defer Free(c_netifaceName) 
	
	var c_sigar_net_interface_stat_t C.sigar_net_interface_stat_t
	if status := int(C.sigar_net_interface_stat_get(GetSigarHandle(), c_netifaceName, &c_sigar_net_interface_stat_t)) ; status != SIGAR_OK {
		return nil,fmt.Errorf("Failed to rertieve network interface status for: %v with error: %v", netIfaceName, status) 
	} 
	
	return &NetIfaceUsageInfo{ 
		RxPackets: uint64(c_sigar_net_interface_stat_t.rx_packets),
	    RxBytes: uint64(c_sigar_net_interface_stat_t.rx_bytes),
	    RxErrors: uint64(c_sigar_net_interface_stat_t.rx_errors),
	    RxDropped: uint64(c_sigar_net_interface_stat_t.rx_dropped),
	    RxOverruns: uint64(c_sigar_net_interface_stat_t.rx_overruns),
	    RxFrame: uint64(c_sigar_net_interface_stat_t.rx_frame),
	    TxPackets: uint64(c_sigar_net_interface_stat_t.tx_packets),
	    TxBytes: uint64(c_sigar_net_interface_stat_t.tx_bytes),
	    TxErrors: uint64(c_sigar_net_interface_stat_t.tx_errors),
	    TxDropped: uint64(c_sigar_net_interface_stat_t.tx_dropped),
	    TxOverruns: uint64(c_sigar_net_interface_stat_t.tx_overruns),
	    TxCollisions: uint64(c_sigar_net_interface_stat_t.tx_collisions),
	    TxCarrier: uint64(c_sigar_net_interface_stat_t.tx_carrier),
	    Speed: uint64(c_sigar_net_interface_stat_t.speed),
	},nil
	
}

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

//----------------------------------------------------------------------------------------
//Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers Helpers  
//----------------------------------------------------------------------------------------

func getNetInterfaceConfigWithSigar(sigar *C.sigar_t) (result []*InterfaceConfigInfo) { 
	
	goInterfacesNames, length, closeable := getNetInterfaceNamesWithSigar(sigar) 
	defer closeable(sigar)
	result = make([]*InterfaceConfigInfo, length) 
	
	var sigarNetInterfaceConfig C.sigar_net_interface_config_t
	for i:=0;i<length;i++{
		
//		fmt.Println("interface name: ", &goInterfacesNames[i])
		C.sigar_net_interface_config_get(sigar, goInterfacesNames[i], &sigarNetInterfaceConfig)
		
		result[i] = &InterfaceConfigInfo{ 
			Name :  C.GoString(&sigarNetInterfaceConfig.name[0]),
			Type :  C.GoString(&sigarNetInterfaceConfig._type[0]),
			Description: C.GoString(&sigarNetInterfaceConfig.description[0]),
			Hwaddr : GetNetAddress(sigar, sigarNetInterfaceConfig.hwaddr), 
			Address : GetNetAddress(sigar, sigarNetInterfaceConfig.address), 
			Destination : GetNetAddress(sigar, sigarNetInterfaceConfig.destination), 
			Broadcast : GetNetAddress(sigar, sigarNetInterfaceConfig.broadcast), 
			Netmask : GetNetAddress(sigar, sigarNetInterfaceConfig.netmask), 
			Address6 : GetNetAddress(sigar, sigarNetInterfaceConfig.address6), 
			Prefix6Length : int(sigarNetInterfaceConfig.prefix6_length), 
			Scope6 : int(sigarNetInterfaceConfig.scope6),
			Flags : uint64(sigarNetInterfaceConfig.flags), 
			Mtu : uint64(sigarNetInterfaceConfig.mtu),
			Metric : uint64(sigarNetInterfaceConfig.metric), 
			TxQueueLen : int(sigarNetInterfaceConfig.tx_queue_len),
		}
	}
	
	return result 
}

type byteSliceReader struct {
   	remain []byte
}
func (br *byteSliceReader) Read(p []byte) (int, error) {
	n := copy(p, br.remain)
   	br.remain = br.remain[n:]
   	return n, nil
}

func GetNetAddress(sigar *C.sigar_t, sigarNetAddress C.sigar_net_address_t) (result *NetAddress) { 
	
	//addstr := C.addr2string(sigar, &sigarNetAddress)
	//fmt.Println("%v", C.GoString(&addstr[0]))
	return &NetAddress{}
	/*var addrStr *[46]*C.char 
	C.sigar_net_address_to_string(sigar, sigarNetAddress, &addrStr[0])
	fmt.Println(C.GoString(&addrStr[0])) 
	
	result = &NetAddress{}
	
		//Family : C.GoString(&sigarNetAddress[0]),
	//addrPointer := (*int32)(unsafe.Pointer(&sigarNetAddress.addr[0]))
	bsr := &byteSliceReader { sigarNetAddress.addr[:] }
	var r io.Reader = bsr 
	var in, in6 C.uint
	binary.Read(r, binary.BigEndian, &in) 
	//fmt.Println("remaining ", len(bsr.remain))
	binary.Read(r, binary.BigEndian, &in6) 
	//fmt.Println("remaining ", len(bsr.remain))
	
	result.In = uint32(in) 
	result.In6 = uint32(in6) 
	
	//mac := (*[]*C.uchar)(unsafe.Pointer(&bsr.remain[0]))
	for i:=0; i < len(bsr.remain) ; i++ { 
		result.Mac = fmt.Sprintf("%v%v", result.Mac, rune(C.char(bsr.remain[i])))
	} 
	//result.Mac = string(mac)
	
	return result 
	*/
}

func getNetInterfaceNamesWithSigar(sigar *C.sigar_t) (goInterfacesNames ([]*C.char), length int, closeable func(sigar *C.sigar_t)) { 
	var netlistconf C.sigar_net_interface_list_t
	
	C.sigar_net_interface_list_get(sigar, &netlistconf)
	
	length =int(netlistconf.number)
	
	cinteraces := GetGoSlice(length, unsafe.Pointer(netlistconf.data))
	goInterfacesNames =  *((*[]*C.char)(unsafe.Pointer(&cinteraces)))	
	
	//goInterfacesNames = ((*[1 << 30]*C.char)(unsafe.Pointer(netlistconf.data)))[:length]
		
	return goInterfacesNames, 
		   length, 
		   func(sigar *C.sigar_t) { 
				C.sigar_net_interface_list_destroy(sigar, &netlistconf)
			}
}

