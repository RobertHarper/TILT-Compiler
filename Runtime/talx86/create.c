#include "s.h"
#include "r.h"
#include "talx86.h"

val_t
get_record(ptr_t rec, int which)
{
	return rec[which];
}

ptr_t
alloc_record(val_t* fields, int count)
{
	if(count == 0)
		return empty_record;
	else {
		size_t size = 4*count;
		ptr_t rec = emalloc_atomic(size);
		int i;

		for (i=0; i<count; i++)
			rec[i] = fields[i];

		return rec;
	}
}

string
alloc_uninit_string(int strlen, char **raw)
{
	int hsize = sizeof(struct str_internal);
	int bufsize = hsize + strlen;
	void* buf = emalloc_atomic(bufsize);
	talx86_string hdr = (talx86_string)buf;
	char* str = (char*)buf + hsize;
	hdr->size = strlen;
	hdr->elts = str;
	*raw = str;
	return (string)hdr;
}

void
adjust_stringlen(string str, int newByteLen)
{
	talx86_string s = (talx86_string)str;
	assert(newByteLen <= s->size);
	s->size=newByteLen;
}

int
stringlen(string str)
{
	talx86_string s = (talx86_string)str;
	return s->size;
}

char*
stringbuf(string str)
{
	talx86_string s = (talx86_string)str;
	return s->elts;
}

/*
	The talx86 representation of type array option (including string
	option) is:

	NONE	a NULL pointer
	SOME a	a pointer to a record [a] (rather than the pointer a)

	Leaf explains:
	>What I want to have is the array type be
	>
	>Ptr(Typecase (rep) of BoxedFloat => Array64 Float
	>                   | _ => Array32 (interp rep))
	>
	>So that I can lump arrays into the "pointer" case of the datatype, and
	>give array option it's natural representation.
	>
	>But TAL forces me to lump together the "Ptr" and the "Array" portion:
	>
	>Typecase (rep) of BoxedFloat => Ptr(Array64 Float)
	>                   | _ => Ptr(Array32 (interp rep))
	>
	>So I have to treat arrays as taglike (since the top level structure
	>doesn't look like a pointer to the type system.  There is a compiler
	>control flag which turns this on and off, and the LIL will work with
	>either setting.  But TAL can only deal with taglike arrays.
*/

ptr_t
valof_arrayopt(option arrayopt)
{
	return (ptr_t) arrayopt[0];
}

option
some_array(ptr_t array)
{
	ptr_t rec = emalloc(4);
	rec[0] = (val_t) array;
	assert(array != NULL);
	return rec;
}

/*
	Use one (expandable) buffer.
*/
static int
expandsize(int size, int request)
{
	while(size < request)
		size *= 2;
	return size;
}

char*
mlstring2cstring_static(string mlstring)
{
	static char* buf = 0;
	static int bufsize = 1;
	int newbufsize = expandsize(bufsize,stringlen(mlstring)+1);
	if(bufsize != newbufsize || buf==0){
		bufsize = newbufsize;
		buf = (char*)erealloc(buf, newbufsize);
	}
	return mlstring2cstring_buffer(mlstring,bufsize,buf);
}
