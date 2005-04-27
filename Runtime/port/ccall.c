/* ../../Basis/ccall.sml */

#include "s.h"
#include "r.h"

/*
	Must agree with ../../Basis/ccall.sml
*/
struct cerr{
	int*	tag;
	uct*	carried;
};
enum { Tok, Terrno, Terrmsg };

void
send_errno(cerr er, int e)
{
	*er->tag = Terrno;
	*er->carried = (uct)e;
}

void
send_errmsg(cerr er,  char* msg)
{
	*er->tag = Terrmsg;
	*er->carried = (uct)msg;
}

string
ccall_errmsg(uct p)
{
	char* msg = (char*)p;
	return cstring2mlstring_alloc(msg);
}
