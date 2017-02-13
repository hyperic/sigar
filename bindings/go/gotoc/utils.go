package gotoc

import (
	"unsafe"
)

/*
#include <stdlib.h>
*/
import "C"

const (
	SIGAR_OK = 0
)

func SigarStringToGoString(cstring *C.char) string{

	var theString string = C.GoString(cstring)  
	
	defer C.free(unsafe.Pointer(cstring))
	return theString

}

func Free(cstring *C.char) { 
	C.free(unsafe.Pointer(cstring)) 
}

