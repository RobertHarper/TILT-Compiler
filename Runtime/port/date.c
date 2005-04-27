/* ../../Basis/date.sml */

#include "s.h"
#include "r.h"
#include <time.h>

static char Emktime[] = "date_mktime: invalid tm";

/*
	Tmrep and tmset must agree with
	type tmrep in ../../Basis/externtys.sml
*/

static ptr_t
Tmrep(struct tm* tm)
{
	val_t fields[9];
	fields[0] = (val_t) tm->tm_sec;
	fields[1] = (val_t) tm->tm_min;
	fields[2] = (val_t) tm->tm_hour;
	fields[3] = (val_t) tm->tm_mday;
	fields[4] = (val_t) tm->tm_mon;
	fields[5] = (val_t) tm->tm_year;
	fields[6] = (val_t) tm->tm_wday;
	fields[7] = (val_t) tm->tm_yday;
	fields[8] = (val_t) tm->tm_isdst;
	return alloc_record(fields, arraysize(fields));
}

static struct tm*
tmset(struct tm* tm, ptr_t tmrep)
{
	tm->tm_sec = tmrep[0];
	tm->tm_min = tmrep[1];
	tm->tm_hour = tmrep[2];
	tm->tm_mday = tmrep[3];
	tm->tm_mon = tmrep[4];
	tm->tm_year = tmrep[5];
	tm->tm_wday = tmrep[6];
	tm->tm_yday = tmrep[7];
	tm->tm_isdst = tmrep[8];
	return tm;
}

string
date_asctime(ptr_t tmrep)
{
	struct tm tm;
	char *cstring = asctime(tmset(&tm, tmrep));
	string mlstring = cstring2mlstring_alloc(cstring);
	return mlstring;
}

ptr_t
date_localtime(cerr er, int time)
{
	time_t t = (time_t)time;
	struct tm* tm = localtime(&t);
	if(tm==NULL)
		send_errno(er,errno);
	return Tmrep(tm);
}

ptr_t
date_gmtime(cerr er, int time)
{
	time_t t = (time_t)time;
	struct tm* tm = gmtime(&t);
	if(tm==NULL)
		send_errno(er,errno);
	return Tmrep(tm);
}

int
date_mktime(cerr er, ptr_t tmrep)
{
	struct tm tm;
	time_t time = mktime(tmset(&tm,tmrep));
	if(time == (time_t)-1)
		send_errmsg(er,Emktime);
	return (int)time;
}

static string
do_strftime(char* buf, int size, char* fmt, struct tm* tm)
{
	if(strftime(buf,size,fmt,tm) == 0) {
		int n = size * 2;
		char* newbuf = (char*)erealloc(buf,n);
		return do_strftime(newbuf,n,fmt,tm);
	} else {
		string formatted = cstring2mlstring_alloc(buf);
		efree(buf);
		return formatted;
	}
}

string
date_strftime(string s, ptr_t tmrep)
{
	char* fmt = mlstring2cstring_static(s);
	struct tm tm;
	int n = 128;
	char* buf = (char*)emalloc_atomic(n);
	return do_strftime(buf,n,fmt,tmset(&tm,tmrep));
}
