package gotoc

import (
	
	"unsafe"
)


//#include <stdlib.h>
import "C"


func SigarStringToGoString(cstring *C.char) string{

	var theString string = C.GoString(cstring)  
	
	defer C.free(unsafe.Pointer(cstring))
	return theString

}