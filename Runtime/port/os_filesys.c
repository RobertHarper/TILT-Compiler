/* ../../Basis/Unix/os-filesys.sml */

#include "s.h"
#include "r.h"

#ifdef AVOID_TMPNAM
	/*string*/cresult
	posix_os_tmpname(unit unused)
	{
		char buf[] = "/tmp/tnXXXXXX";
		int fd = mkstemp(buf);
		if(fd == -1)
			return Error(SysErr(errno));
		else {
			string name = cstring2mlstring_alloc(buf);
			close(fd);
			return NormalPtr(name);
		}
	}
#else
	/*string*/cresult
	posix_os_tmpname(unit unused)
	{
		char* buf;
		string res = alloc_uninit_string(L_tmpnam,&buf);
		char *result = tmpnam(buf);
		assert(result == buf);
		adjust_stringlen(res,strlen(buf));
		return res;
	}
#endif
