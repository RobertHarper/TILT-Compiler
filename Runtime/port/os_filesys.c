/* ../../Basis/Unix/os-filesys.sml */

#include "s.h"
#include "r.h"

string
os_filesys_tmpname(cerr er)
{
	#ifdef AVOID_TMPNAM
		char buf[] = "/tmp/tnXXXXXX";
		int fd = mkstemp(buf);
		if(fd == -1){
			send_errno(er,errno);
			*buf = 0;
		}
		else
			close(fd);
		return cstring2mlstring_alloc(buf);
	#else
		char* buf;
		string res = alloc_uninit_string(L_tmpnam,&buf);
		char *result = tmpnam(buf);
		assert(result == buf);
		adjust_stringlen(res,strlen(buf));
		return res;
	#endif
}
