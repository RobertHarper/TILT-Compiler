#include "s.h"
#include "r.h"

enum {
	/* The fields of an exn record. */
	Stamp=0, Arg, Name,

	BadStamp = ~1,
};

static val_t	subStamp = BadStamp;
static val_t	divStamp = BadStamp;
static val_t	ovflStamp = BadStamp;
static val_t	sysErrStamp = BadStamp;
static val_t	libFailStamp = BadStamp;

static val_t
get_stamp(exn e)
{
	return get_record(e,Stamp);
}

static val_t
get_arg(exn e)
{
	return get_record(e,Arg);
}

string
exnNameRuntime(exn e)
{
	return (string)get_record(e,Name);
}

/*
	The Basis library should use the following to tell the runtime
	about some key ML level exceptions.
*/

unit
registerSubExnRuntime(exn exn)
{
	subStamp = get_stamp(exn);
	return empty_record;
}

unit
registerDivExnRuntime(exn exn)
{
	divStamp = get_stamp(exn);
	return empty_record;
}

unit
registerOvflExnRuntime(exn exn)
{
	ovflStamp = get_stamp(exn);
	return empty_record;
}
unit
registerSysErrExnRuntime(exn exn)
{
	sysErrStamp = get_stamp(exn);
	return empty_record;
}

unit
registerLibFailExnRuntime(exn exn)
{
	libFailStamp = get_stamp(exn);
	return empty_record;
}

static val_t
getSubStamp()
{
	if(subStamp == BadStamp)
		DIE("SubStamp uninitialized");
	return subStamp;
}

static val_t
getDivStamp()
{
	if(divStamp == BadStamp)
		DIE("DivStamp uninitialized");
	return divStamp;
}

static val_t
getOvflStamp()
{
	if(ovflStamp == BadStamp)
		DIE("OverflowStamp uninitialized");
	return ovflStamp;
}

static val_t
getSysErrStamp()
{
	if(sysErrStamp == BadStamp)
		DIE("SysErrStamp uninitialized");
	return sysErrStamp;
}

static val_t
getLibFailStamp()
{
	if(libFailStamp == BadStamp)
		DIE("libFailStamp uninitialized");
	return libFailStamp;
}

static exn
mkExn(string exnname, val_t exnstamp, val_t exnarg, bool argpointer)
{
	val_t fields[3];
	int mask = (1<<Name) | (argpointer ? (1<<Arg) : 0);
	exn e;
	fields[Stamp] = exnstamp;
	fields[Arg]  = exnarg;
	fields[Name]  = (val_t)exnname;
	e = alloc_record(fields, mask, 3);
	return e;
}

exn
mkDivExn(void)
{
	string exnname = cstring2mlstring_alloc("Div");
	val_t exnstamp = getDivStamp();
	exn e = mkExn(exnname,exnstamp,(val_t)empty_record,false);
	return e;
}

exn
mkOverflowExn(void) 
{
	string exnname = cstring2mlstring_alloc("Overflow");
	val_t exnstamp = getOvflStamp();
	exn e = mkExn(exnname,exnstamp,(val_t)empty_record,false);
	return e;
}

exn
mkSubscriptExn(void)
{
	string exnname = cstring2mlstring_alloc("Subscript");
	val_t exnstamp = getSubStamp();
	exn e = mkExn(exnname,exnstamp,(val_t)empty_record,false);
	return e;
}

static exn
mkSysErrExn(string msg, ptr_t errno_option)
{
	string exnname = cstring2mlstring_alloc("SysErr");
	val_t exnstamp = getSysErrStamp();
	ptr_t exnarg = alloc_ptrptr(msg, errno_option);
	exn e = mkExn(exnname,exnstamp,(val_t)exnarg,true);
	return e;
}

static ptr_t
some_int_option(int32 i)
{
	val_t fields[1];
	fields[0] = (val_t) i;
	return alloc_record(fields, 0, 1);
}

exn
SysErr(int e)
{
	char* cmsg = strerror(e);
	string msg = cstring2mlstring_alloc(cmsg);
	ptr_t errno_option = some_int_option(e);
	exn exn = mkSysErrExn(msg,errno_option);
	return exn;
}

exn
SysErr_msg(char* cmsg)
{
	string msg = cstring2mlstring_alloc(cmsg);
	ptr_t errno_option = 0;
	exn exn = mkSysErrExn(msg,errno_option);
	return exn;
}

exn
SysErr_fmt(char* fmt, ...)
{
	char buf[1024];
	va_list args;
	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);
	return SysErr_msg(buf);
}

string
exnMessageRuntime(exn exn)
{
	char buf[1024];
	val_t exnstamp = get_stamp(exn);

	if (exnstamp == getDivStamp()) {
		strcpy(buf, "divide by zero");
	} else if (exnstamp == getOvflStamp()) {
		strcpy(buf, "overflow");
	} else if (exnstamp == getLibFailStamp()) {
		const char* prefix = "LibFail: ";
		string msg = (string) get_arg(exn);
		int msg_len = stringlen(msg);
		assert(sizeof(buf) > sizeof(prefix) + msg_len);
		sprintf(buf, "%s%.*s", prefix, msg_len, stringbuf(msg));
	} else if (exnstamp == getSysErrStamp()) {
		ptr_t exnarg = (ptr_t)get_arg(exn);
		const char* prefix = "SysErr: ";
		string msg = (string) get_record(exnarg, 0);
		int msg_len = stringlen(msg);
		ptr_t err_option = (ptr_t) get_record(exnarg, 1);
		int isSome = err_option != 0;
		int err = isSome ? (int)get_record(err_option, 0) : 0;
		char err_buf[40];
		if (isSome) {
			sprintf(err_buf, " (errno=%d)", err);
		} else {
			*err_buf = '\0';
		}
		assert(sizeof(buf) > sizeof(prefix) + msg_len + strlen(err_buf));
		sprintf(buf, "%s%.*s%s", prefix, msg_len, stringbuf(msg), err_buf);
	} else {
		string name = exnNameRuntime(exn);
		int name_len = stringlen(name);
		assert(sizeof(buf) > name_len);
		sprintf(buf, "%.*s", name_len, stringbuf(name));
	}
	return cstring2mlstring_alloc(buf);
}
