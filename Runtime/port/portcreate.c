#include "s.h"
#include "r.h"

static record
alloc_pair(val_t rec1, val_t rec2, int mask)
{
	val_t fields[2];
	fields[0] = rec1;
	fields[1] = rec2;
	return alloc_record(fields, mask, 2);
}

record
alloc_ptrptr(ptr_t rec1, ptr_t rec2)
{
	return alloc_pair((val_t)rec1, (val_t)rec2, (1<<0)|(1<<1));
}

static record
alloc_intptr(val_t rec1, ptr_t rec2)
{
	return alloc_pair(rec1, (val_t)rec2, 1<<1);
}

record
alloc_intint(val_t rec1, val_t rec2)
{
	return alloc_pair(rec1, rec2, 0);
}

string
alloc_string(int bufsize, char* buf)
{
	char* raw;
	string res = alloc_uninit_string(bufsize,&raw);
	memcpy(raw,buf,bufsize);
	return res;
}
string
cstring2mlstring_alloc(char *str)
{
	return alloc_string(strlen(str),str);
}

char*
mlstring2cstring_buffer(string mlstring, int bufsize, char* buf)
{
	int bytelen = stringlen(mlstring);
	char *raw = stringbuf(mlstring);
	if((bytelen+1) > bufsize)
		DIE("buffer too small for string");
	memcpy(buf,raw,bytelen);
	buf[bytelen] = 0;
	return buf;
}

char*
mlstring2cstring_malloc(string mlstring)
{
	int size = stringlen(mlstring) + 1;
	char* buf = (char*)emalloc_atomic(size);
	return mlstring2cstring_buffer(mlstring, size, buf);
}

string_option
alloc_string_option(char* cstringopt)
{
	return cstringopt
		? some_array(cstring2mlstring_alloc(cstringopt))
		: NULL;
}

ptr_t
stringpair_ctoml_alloc(char* a, char* b)
{
	string ml_a = cstring2mlstring_alloc(a);
	string ml_b = cstring2mlstring_alloc(b);
	return alloc_ptrptr(ml_a, ml_b);
}

list
cons_ptr_alloc(ptr_t car, list list)
{
	return alloc_ptrptr(car, list);
}

list
cons_int_alloc(val_t car, list list)
{
	return alloc_intptr(car, list);
}

char**
string_list_to_array_malloc(string_list list)
{
	char** v;
	char* cursor;
	string_list_long L;
	int i;
	int listlen = 0;
	int textsize = 0;

	for(L=(string_list_long)list; L != NULL; L = (string_list_long)L->cdr) {
		string mlstring = L->car;
		int size = stringlen(mlstring) + 1;
		listlen ++;
		textsize += size;
	}
	v = (char**)emalloc_atomic(sizeof(char*) * (listlen + 1) + textsize);
	cursor = (char*)(v + listlen + 1);
	for(i=0, L=(string_list_long)list; i<listlen; i++, L = (string_list_long)L->cdr) {
		string mlstring = L->car;
		int size = stringlen(mlstring) + 1;
		v[i] = mlstring2cstring_buffer(mlstring, size, cursor);
		cursor += size;
	}
	v[listlen] = 0;
	return v;
}

string_list
array_to_string_list(char** arr)
{
	char* string = *arr;
	char** next = arr+1;
	if (string == NULL) return (ptr_t) 0;
	else {
		ptr_t car = (ptr_t) cstring2mlstring_alloc(string);
		ptr_t cdr = array_to_string_list(next);
		return cons_ptr_alloc(car,cdr);
	}
}

/*
	Must agree with Basis' datatype cresult.
	Note that the tags are sorted by TILT.
*/
enum { ErrorTag, NormalTag };

cresult
Error(ptr_t exn)
{
	return alloc_intptr((val_t)ErrorTag, exn);
}

cresult
Normal(val_t v)
{
	return alloc_intint((val_t)NormalTag, v);
}

cresult
NormalPtr(ptr_t v)
{
	return alloc_intptr((val_t)NormalTag, v);
}

/*unit*/cresult
unit_cresult(int result)
{
	if(result==-1)
		return Error(SysErr(errno));
	else
		return NormalPtr(empty_record);
}
