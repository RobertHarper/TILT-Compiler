#include "s.h"
#include "r.h"

record
alloc_intint(val_t rec1, val_t rec2)
{
	val_t fields[2];
	fields[0] = rec1;
	fields[1] = rec2;
	return alloc_record(fields, 2);
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
