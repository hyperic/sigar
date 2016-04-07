package gotoc


import (
	
	
	
	
)
/*

#include "../../../../../Include/sigar.h"



*/
import "C"

func NetUtils(flags int)(C.sigar_net_stat_t,
 C.sigar_tcp_t,
 C.sigar_nfs_client_v2_t,
 C.sigar_nfs_server_v2_t,
 C.sigar_nfs_client_v3_t,
 C.sigar_nfs_server_v3_t){

	var sigar *C.sigar_t=GetSigarHandle()
	var netstat C.sigar_net_stat_t 
	var tcp C.sigar_tcp_t 
	var nfs2 C.sigar_nfs_client_v2_t
	var nfsServer2 C.sigar_nfs_server_v2_t
	var nfs3 C.sigar_nfs_client_v3_t
	var nfsServer3 C.sigar_nfs_server_v3_t
	
	C.sigar_net_stat_get( sigar,&netstat,C.int(flags))
          
    C.sigar_tcp_get(sigar,&tcp)
    
    C.sigar_nfs_client_v2_get(sigar, &nfs2)
    C.sigar_nfs_server_v2_get (sigar, &nfsServer2)      
    C.sigar_nfs_client_v3_get(sigar, &nfs3)
    C.sigar_nfs_server_v3_get (sigar, &nfsServer3)      
    return netstat, tcp, nfs2,nfsServer2, nfs3,nfsServer3
                    
                    
}