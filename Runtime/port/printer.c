/* ../../Basis/printer.sml */

#include "s.h"
#include "r.h"

unit
printer_print(cerr er, string s) 
{
	int n = stringlen(s);
	char* buf = stringbuf(s);
	if(fwrite(buf,1,n,stdout) != n)
		send_errno(er,errno);
	return empty_record;
}
