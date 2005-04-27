#include "s.h"
#include "r.h"
#include "talx86.h"

static exn
mkExn(string exnname, val_t exnstamp)
{
	ptr_t rec = emalloc(12);
	rec[0] = exnstamp;
	rec[1] = (val_t)empty_record;
	rec[2] = (val_t)exnname;
	return rec;
}

exn
mkDivExn(void)
{
	string exnname = cstring2mlstring_alloc("Div");
	val_t exnstamp = getDivStamp();
	exn e = mkExn(exnname,exnstamp);
	return e;
}

exn
mkOverflowExn(void) 
{
	string exnname = cstring2mlstring_alloc("Overflow");
	val_t exnstamp = getOvflStamp();
	exn e = mkExn(exnname,exnstamp);
	return e;
}

exn
mkSubscriptExn(void)
{
	string exnname = cstring2mlstring_alloc("Subscript");
	val_t exnstamp = getSubStamp();
	exn e = mkExn(exnname,exnstamp);
	return e;
}
