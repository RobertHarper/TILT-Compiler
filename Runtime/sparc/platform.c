#include "s.h"
#include "r.h"
#include "sparc.h"

int
GetBcacheSize(void)
{ 
	return 512 * 1024; 
}

int
GetIcacheSize(void)
{ 
	return 8 * 1024; 
}

int
GetDcacheSize(void)
{ 
	return 8 * 1024; 
}

int
GetPhysicalPages(void)
{
	long r = sysconf(_SC_PHYS_PAGES);
	if (r == -1) {
		perror("sysconf");
		exit(1);
	}
	return r;
}
