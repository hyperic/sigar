package gotoc

import (

	"fmt"
	"reflect"
	"unsafe"

)

/*
#include "../../../../../Include/sigar.h"

*/
import "C"

var sigar *C.sigar_t=nil
func GetSigarHandle()*C.sigar_t{

	if(sigar!=nil){
		return sigar
	}
	sigar=new (C.sigar_t)
	ret:=C.sigar_open(&sigar)
	if(ret!=C.SIGAR_OK){
		return nil
	}
	return sigar

}

func CloseSigarHandle(sigar *C.sigar_t) {

	if(sigar!=nil){
		C.sigar_close(sigar)
		return
	}
	fmt.Println("Trying to close a nil handel, ignoring")
}


func GetGoSlice(number int, pointer unsafe.Pointer ) reflect.SliceHeader{
	var length int=int(number)
	
	
	cObj := reflect.SliceHeader{
        Data: uintptr(pointer),
        Len:  length,
        Cap:  length,
    }
	
	return cObj
}

 
func CArr2SlicePtr(length int, cArray interface{}) unsafe.Pointer{
	
	vl := reflect.ValueOf(cArray)
	header := reflect.SliceHeader{
        Data: uintptr(unsafe.Pointer(vl.Pointer())),
        Len:  length,
        Cap:  length,
    }
	
	return unsafe.Pointer(&header)
}