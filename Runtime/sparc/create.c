#include "s.h"
#include "r.h"
#include "sparc.h"

/*
	The sparc GC does not know about C roots.  Sparc C calls may
	perform at most one allocation and must consider any other ML
	pointers on hand to be invalidated by allocation.

	In order to avoid a nightmare debugging session, alloc_space
	aborts rather than let a C call allocate a second object.  It
	uses the thread-local variable safeToGC that is set true when
	transitioning from ML to C.
*/

static mem_t
alloc_space(int bytesNeeded)
{
	/*
		XXX should allocate large request from largespace
	*/
	mem_t alloc, limit, newAlloc;
	Thread_t *th = getThread();
	int wordsNeeded = bytesNeeded / 4;
	if (th != NULL) {
		if(th->safeToGC)
			th->safeToGC = 0;
		else
			DIE("runtime allocated multiple objects");
		alloc = (mem_t) th->saveregs[ALLOCPTR];
		limit = (mem_t) th->saveregs[ALLOCLIMIT];
		newAlloc = alloc + wordsNeeded;
		if (newAlloc <= limit) {
			th->saveregs[ALLOCPTR] = (unsigned long) newAlloc;
			return alloc;
		}
		else {
			GCFromC(th,bytesNeeded,0);
			alloc = (mem_t) th->saveregs[ALLOCPTR];
			limit = (mem_t) th->saveregs[ALLOCLIMIT];
			newAlloc = alloc + wordsNeeded;
			assert(newAlloc <= limit);  /* This time the request must have been satisfied. */
			th->saveregs[ALLOCPTR] = (unsigned long) newAlloc;
			return alloc;
		}
	}
	else {
		alloc = (mem_t) RuntimeGlobalData_Cur;
		limit = (mem_t) RuntimeGlobalData_End;
		newAlloc = alloc + wordsNeeded;
		assert(newAlloc <= limit);
		RuntimeGlobalData_Cur = newAlloc;
		fprintf(stderr,"Warning: alloc_space called with no thread mapped\n");
		return alloc;
	}
}

val_t
get_record(ptr_t rec, int which)
{
	assert(GET_TYPE(rec[-1]) == RECORD_TYPE);
	assert(which < GET_RECLEN(rec[-1]));
	return rec[which];
}

ptr_t
alloc_record(val_t* fields, int count)
{
	int mask = 0;	/* pointer fields are unsafe due to possibility of gc from c */
	int i;
	mem_t alloc;
	ptr_t rec;

	assert(count <= RECLEN_MAX);

	if (count == 0)
		return empty_record;

	alloc = alloc_space(4 * (count + 1));
	rec = alloc + 1;

	rec[-1] = RECORD_TYPE
		| (count << RECLEN_OFFSET)
		| (mask << RECMASK_OFFSET);

	for (i=0; i<count; i++)
		rec[i] = fields[i];

	return rec;
}

static int
num_segments(int byteLen)
{
	return (arraySegmentSize == 0 || byteLen <= arraySegmentSize)
		? 0
		: DivideUp(byteLen, arraySegmentSize);
}

string
alloc_uninit_string(int strLen, char **raw)
{
	int wordLen = (strLen + 3) / 4;
	int byteLen = 4 * wordLen;
	int segments = num_segments(byteLen);
	int totalByteLen = byteLen + 4 + 4 * segments;
	int tag = WORD_ARRAY_TYPE | (strLen << ARRLEN_OFFSET);
	mem_t alloc = alloc_space(totalByteLen);
	ptr_t res = alloc + 1 + segments;
	int i;

	for (i=0; i<segments; i++)
		res[-(2+i)] = SEGPROCEED_TAG;
	res[-1] = tag;
	*raw = (char *)res;
	return res;
}

void
adjust_stringlen(string str, int newByteLen)
{
	int i;
	tag_t oldTag = str[-1];
	tag_t newTag = WORD_ARRAY_TYPE | (newByteLen << ARRLEN_OFFSET);
	int oldByteLen = GET_ANY_ARRAY_LEN(oldTag);
	int oldWordLen = (oldByteLen + 3) / 4;
	int newWordLen = (newByteLen + 3) / 4;
	int oldSegments = num_segments(oldByteLen);
	int newSegments = num_segments(newByteLen);

	assert(GET_TYPE(oldTag) == WORD_ARRAY_TYPE);
	assert(newByteLen <= oldByteLen);

	/* Fill the excess space (tag and data) with SKIP_TYPE. */
	if (newWordLen != oldWordLen) {
		for (i=newSegments; i<oldSegments; i++)
			str[-(2+i)] = MAKE_SKIP(1);
		str[newWordLen] = MAKE_SKIP(oldWordLen - newWordLen);
	}
	str[-1] = newTag;
}

int
stringlen(string s)
{
	tag_t tag = s[-1];
	int len;
	while (TYPE_IS_FORWARD(GET_TYPE(tag)))
		tag = ((ptr_t)tag) [-1];
	assert(GET_TYPE(tag) == WORD_ARRAY_TYPE);
	len = GET_WORD_ARRAY_LEN(tag);
	return len;
}

char*
stringbuf(string s)
{
	return (char*)s;
}

void
init_iarray(ptr_t obj, int byteLen, int v)
{
	int i;
	int tag = WORD_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
	obj[-1] = tag;
	for (i=0; i<(byteLen + 3) / 4; i++)
		obj[i] = v;
}

void
init_parray(ptr_t obj, int len, ptr_t v)
{
	int i;
	int byteLen = 4 * len;
	int tag = PTR_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
	obj[-1] = tag;
	for (i=0; i<len; i++)
		obj[i] = (val_t) v;
}

void
init_double_ptr_array(ptr_t obj, int logLen, ptr_t v)
{
	int i;
	int wordLen = 2 * logLen;
	int byteLen = 4 * wordLen;
	int tag = MIRROR_PTR_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
	obj[-1] = tag;
	for (i=0; i<wordLen; i++)   /* Initialize primary and replica fields */
		obj[i] = (val_t) v;
}

void
init_farray(ptr_t obj, int len, double v)
{
	int i;
	int byteLen = 8 * len;
	int tag = QUAD_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
	assert((((unsigned int)obj) & 7) == 0);
	obj[-1] = tag;
	for (i=0; i<len; i++)
		((double *)obj)[i] = v;
}

ptr_t
valof_arrayopt(option arrayopt)
{
	assert(arrayopt != NULL);
	return arrayopt;
}

option
some_array(ptr_t array)
{
	assert(array != NULL);
	return array;
}

/*
	Use one (expandable) buffer per proc.
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
	Proc_t* proc = getProc();
	int newbufsize = expandsize(proc->bufsize,stringlen(mlstring)+1);
	if(proc->bufsize != newbufsize || proc->buf==0){
		proc->bufsize = newbufsize;
		proc->buf = (char*)erealloc(proc->buf, newbufsize);
	}
	return mlstring2cstring_buffer(mlstring,proc->bufsize,proc->buf);
}
