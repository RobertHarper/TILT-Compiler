#include "general.h"
#include <string.h>
#include "tag.h"
#include "thread.h"
#include "global.h"
#include "create.h"
#include "gc.h"

int exncounter = 4000;
const ptr_t empty_record = (ptr_t) 256; /* This is the ML unit. */

mem_t oddword_align(mem_t ptr)
{
  unsigned int v = (unsigned int) ptr;
  if ((v & 7) == 0) 
    *(ptr++) = MAKE_SKIP(1);
  return ptr;
}

mem_t evenword_align(mem_t ptr)
{
  unsigned int v = (unsigned int) ptr;
  if ((v & 7) != 0) 
    *(ptr++) = MAKE_SKIP(1);
  return ptr;
}

static mem_t alloc_space(int bytesNeeded) 
{				/* XXX should allocate large request from largespace */
  mem_t alloc, limit, newAlloc;
  Thread_t *th = getThread();
  int wordsNeeded = bytesNeeded / 4;
  if (th != NULL) {
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
    assert(0);
/*
    alloc = (mem_t) RuntimeGlobalData_Cur;
    limit = (mem_t) RuntimeGlobalData_End;
    newAlloc = alloc + wordsNeeded;
    assert(newAlloc <= limit);  
    RuntimeGlobalData_Cur = newAlloc;
    return alloc;
*/
  }
}


ptr_t alloc_iarray(int count, int n)
{
  ptr_t obj = 0;
  int len = count ? count : 1;
  int mask = 0;
  mem_t alloc = alloc_space(4 * (len + 1));

  obj = alloc + 1;
  obj[-1] = WORD_ARRAY_TYPE | (count << (2 + ARRLEN_OFFSET));
  while (len > 0)
    obj[--len]  = n;
  
  return obj;
}

ptr_t alloc_rarray(int count, double val)
{
  int len = count ? count : 1;
  int mask = 0;
  ptr_t obj;

  mem_t alloc = alloc_space(8 * (len + 1)); /* tags and alignment */
  alloc = oddword_align(alloc);
  obj = alloc + 1;
  obj[-1] = QUAD_ARRAY_TYPE | (len << (3 + ARRLEN_OFFSET));
  while (count > 0) {
    count--;
    ((double *)obj)[count]  = val;
  }
  obj[2*len] = MAKE_SKIP(1);
  return obj;
}

val_t get_record(ptr_t rec, int which)
{
  tag_t tag = rec[-1];
  int len = GET_RECLEN(tag);

  if (GET_TYPE(tag) != RECORD_TYPE) {
    printf("BUG: calling get_field on non-record. tag = %d\n",tag);
    exit(-1);
  }

  if (which < len)
    return rec[which];
  else {
    printf("BUG in get_record: record %d has %d fields.  No field %d.\n",
	   rec,len,which);
    assert(0);
  }
}

static ptr_t alloc_small_record(val_t *fields, int mask, int count)
{
  /* Bits 0..(RECLEN_MAX - 1) of mask are significant. */
  int i;
  mem_t alloc;
  ptr_t rec;
  
  assert(count <= RECLEN_MAX);
  assert((mask >> count) == 0);
  
  if (count == 0)
    return empty_record;

  alloc = alloc_space(4 * (count + 1));
  rec = alloc + 1;

  rec[-1] = RECORD_TYPE | (count << RECLEN_OFFSET) | (mask << RECMASK_OFFSET);

  /* Initialize record fields */
  for (i=0; i<count; i++)
    rec[i] = fields[i];
      
  return rec;
}


ptr_t alloc_record(val_t *fields, int* masks, int count)
{
  /* For i in 0..(count / RECLEN_MAX),
   * Bits 0..(RECLEN_MAX - 1) of masks[i] are significant.
   */
  if (count > RECLEN_MAX)
    BUG("alloc_record not quite fully imped");

  return alloc_small_record(fields, masks[0], count); 
}

ptr_t alloc_string_help(int strLen)
{
  ptr_t res;
  int wordLen = (strLen + 3) / 4;
  int byteLen = 4 * wordLen;
  int i, segments = (arraySegmentSize == 0 || byteLen <= arraySegmentSize) ? 0 : DivideUp(byteLen, arraySegmentSize);
  int totalByteLen = byteLen + 4 + 4 * segments;
  int tag = WORD_ARRAY_TYPE | (strLen << ARRLEN_OFFSET);

  mem_t alloc = alloc_space(totalByteLen);
  res = alloc + 1 + segments;
  for (i=0; i<segments; i++)
    res[-(2+i)] = SEGPROCEED_TAG;
  res[-1] = tag;

  return res;
}


ptr_t alloc_string(int strlen, char *str)
{
  mem_t res = alloc_string_help(strlen);
  memcpy((char *)res,str,strlen);
  return res;
}


ptr_t alloc_uninit_string(int strlen, char **raw)
{
  mem_t res = alloc_string_help(strlen);
  *raw = (char *)res;
  return res;
}

/* Shorten a string and fill the excess space (tag and data) with SKIP_TYPE */
void adjust_stringlen(ptr_t str, int newByteLen)
{
  int i;
  tag_t oldTag = str[-1];
  tag_t newTag = WORD_ARRAY_TYPE | (newByteLen << ARRLEN_OFFSET);
  int oldByteLen = GET_ANY_ARRAY_LEN(oldTag);
  int oldWordLen = (oldByteLen + 3) / 4;
  int newWordLen = (newByteLen + 3) / 4;
  int oldSegments = (arraySegmentSize == 0 || oldByteLen <= arraySegmentSize) ? 0 : DivideUp(oldByteLen, arraySegmentSize);
  int newSegments = (arraySegmentSize == 0 || newByteLen <= arraySegmentSize) ? 0 : DivideUp(newByteLen, arraySegmentSize);

  assert(GET_TYPE(oldTag) == WORD_ARRAY_TYPE);
  assert(newByteLen <= oldByteLen);
  if (newWordLen != oldWordLen) {
    for (i=newSegments; i<oldSegments; i++)
      str[-(2+i)] = MAKE_SKIP(1);
    str[newWordLen] = MAKE_SKIP(oldWordLen - newWordLen);
  }
  str[-1] = newTag;
}

ptr_t alloc_recrec(ptr_t rec1, ptr_t rec2)
{
  val_t fields[2];
  fields[0] = (val_t) rec1;
  fields[1] = (val_t) rec2;
  return alloc_small_record(fields, 3, 2);
}

ptr_t alloc_manyint(int count, int v)
{
  val_t fields[100];
  int masks[100 / RECLEN_MAX];
  int i;

  if (count > arraysize(fields))
    BUG("alloc_manyint not quite fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;

  for (i=0; i<arraysize(masks); i++)
    masks[i] = 0;
  
  return alloc_record(fields, masks, count);
}

ptr_t alloc_manyintrec(int count, int v, ptr_t rec)
{
  val_t fields[100];
  int masks[100 / RECLEN_MAX];
  int i;

  if (count + 1 > arraysize(fields))
    BUG("alloc_manyintrec not quite fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;
  fields[count] = (val_t) rec;

  for (i=0; i<arraysize(masks); i++)
    masks[i] = 0;
  masks[count/RECLEN_MAX] |= 1 << (count % RECLEN_MAX);
  
  return alloc_record(fields, masks, count+1);
}

ptr_t alloc_intrec(int n, ptr_t rec)
{
  return alloc_manyintrec(1,n,rec);
}

ptr_t alloc_intint(int a, int b)
{
  ptr_t result = alloc_manyint(2,a);
  result[1] = b;
  return result;
}

void init_iarray(ptr_t obj, int byteLen, int v)
{
  int i;
  int tag = WORD_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
  obj[-1] = tag;
  for (i=0; i<(byteLen + 3) / 4; i++)
    obj[i] = v;
}

void init_parray(ptr_t obj, int len, ptr_t v)
{
  int i;
  int byteLen = 4 * len;
  int tag = PTR_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
  obj[-1] = tag;
  for (i=0; i<len; i++)
    obj[i] = (val_t) v;
}

void init_double_ptr_array(ptr_t obj, int logLen, ptr_t v)
{
  int i;
  int wordLen = 2 * logLen;
  int byteLen = 4 * wordLen;
  int tag = MIRROR_PTR_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
  obj[-1] = tag;
  for (i=0; i<wordLen; i++)   /* Initialize primary and replica fields */
    obj[i] = (val_t) v;
}

void init_farray(ptr_t obj, int len, double v)
{
  int i;
  int byteLen = 8 * len;
  int tag = QUAD_ARRAY_TYPE | (byteLen << ARRLEN_OFFSET);
  assert((((unsigned int)obj) & 7) == 0);
  obj[-1] = tag;
  for (i=0; i<len; i++)
    ((double *)obj)[i] = v;
}

