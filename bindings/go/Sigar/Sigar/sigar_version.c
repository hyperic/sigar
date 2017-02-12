#include "sigar.h"

static sigar_version_t sigar_version = {
	"2004-2011",
	"1",
	"7",
	"0",
	"0",
	"sigar",
	"1.66",
	2,
	17122014,
	1,
	6
	
};

SIGAR_DECLARE(sigar_version_t *) sigar_version_get(void)
{
	return &sigar_version;
}
