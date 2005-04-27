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

val_t
getSubStamp(void)
{
	if(subStamp == BadStamp)
		DIE("SubStamp uninitialized");
	return subStamp;
}

val_t
getDivStamp(void)
{
	if(divStamp == BadStamp)
		DIE("DivStamp uninitialized");
	return divStamp;
}

val_t
getOvflStamp(void)
{
	if(ovflStamp == BadStamp)
		DIE("OverflowStamp uninitialized");
	return ovflStamp;
}

static val_t
getSysErrStamp(void)
{
	if(sysErrStamp == BadStamp)
		DIE("SysErrStamp uninitialized");
	return sysErrStamp;
}

static val_t
getLibFailStamp(void)
{
	if(libFailStamp == BadStamp)
		DIE("libFailStamp uninitialized");
	return libFailStamp;
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
	} else if (exnstamp == getSubStamp()) {
		strcpy(buf, "subscript out of bounds");
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
