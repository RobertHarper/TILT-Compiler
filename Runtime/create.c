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
    *(ptr++) = SKIP_TAG | (1 << SKIPLEN_OFFSET);
  return ptr;
}

mem_t evenword_align(mem_t ptr)
{
  unsigned int v = (unsigned int) ptr;
  if ((v & 7) != 0) 
    *(ptr++) = SKIP_TAG | (1 << SKIPLEN_OFFSET);
  return ptr;
}

static mem_t alloc_space(int bytesNeeded) 
{
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
    alloc = (mem_t) RuntimeGlobalData_Cur;
    limit = (mem_t) RuntimeGlobalData_End;
    newAlloc = alloc + wordsNeeded;
    assert(newAlloc <= limit);  /* We should not allocate from global segment too much. */
    RuntimeGlobalData_Cur = newAlloc;
    return alloc;
  }
}


ptr_t alloc_iarray(int count, int n)
{
  ptr_t obj = 0;
  int len = count ? count : 1;
  int mask = 0;
  mem_t alloc = alloc_space(4 * (len + 1));

  obj = alloc + 1;
  obj[-1] = IARRAY_TAG | (count << (2+ARRLEN_OFFSET));
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
  obj[-1] = RARRAY_TAG | (len << (2+ARRLEN_OFFSET));
  while (count > 0) {
    count--;
    ((double *)obj)[count]  = val;
  }
  obj[2*len] = SKIP_TAG | (1 << SKIPLEN_OFFSET);
  return obj;
}

val_t get_record(ptr_t rec, int which)
{
  tag_t tag = rec[-1];
  int len = GET_RECLEN(tag);

  if (!(IS_RECORD(tag))) {
    printf("BUG: calling get_field on non-record. tag = %d\n",tag);
    exit(-1);
  }

  if (which < len)
    return rec[which];
  else
    {
      printf("BUG in get_record: record %d has %d fields.  No field %d.\n",
	     rec,len,which);
      exit(-1);
    }
}

ptr_t alloc_record(val_t *fields, int *masks, int count)
{
  int i;
  mem_t alloc;
  ptr_t rec;
  assert(count <= RECLEN_MAX);
  if (count == 0)
    return empty_record;

  alloc = alloc_space(4 * (count + 1));
  rec = alloc + 1;

  /* Initialize record tag */
  {
    int tag, mask = 0;
    for (i=0; i<count; i++)
      {
	int whichbyte = count >> 5;
	int whichbit = count & 31;
	int ison = masks[whichbyte] & (1 << whichbit);
	if (ison)
	  mask |= 1 << i;
      }
    tag = RECORD_TAG | (count << RECLEN_OFFSET) | (mask << RECMASK_OFFSET);
    rec[-1] = tag;
  }

  /* Initialize record fields */
  for (i=0; i<count; i++)
    rec[i] = fields[i];
      
  return rec;
}



ptr_t alloc_string(int strlen, char *str)

{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  ptr_t res;
  int tag = IARRAY_TAG | (strlen << ARRLEN_OFFSET);

  mem_t alloc = alloc_space(4 * (wordlen + 1));
  res = alloc + 1;
  res[-1] = tag;
  bcopy(str,(char *)res,strlen);

  return res;
}


ptr_t alloc_uninit_string(int strlen, char **raw)
{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  ptr_t res;
  int tag = IARRAY_TAG | (strlen << ARRLEN_OFFSET);
  mem_t alloc = alloc_space(4 * (wordlen + 1));
  res = alloc + 1;
  res[-1] = tag;
  *raw = (char *)res;
  return res;
}

/* Shorten a string and fill the space it used to occupy with SKIP_TAG */
void adjust_stringlen(ptr_t str, int newlen)
{
  int i;
  tag_t oldtag = str[-1];
  tag_t newtag = IARRAY_TAG | (newlen << ARRLEN_OFFSET);
  int oldlen = GET_ARRLEN(oldtag);
  int oldword = (oldlen + 3) / 4;
  int newword = (newlen + 3) / 4;

  assert(newlen <= oldlen);
  str[-1] = newtag;
  str[newword] = SKIP_TAG | ((oldword - newword) << SKIPLEN_OFFSET);
}

ptr_t alloc_recrec(ptr_t rec1, ptr_t rec2)
{
  val_t fields[2];
  int mask = 3;
  fields[0] = (val_t) rec1;
  fields[1] = (val_t) rec2;

  return alloc_record(fields, &mask, 2);
}


ptr_t alloc_manyint(int count, int v)
{
  int masks[1 + (100/RECLEN_MAX)];
  val_t fields[100]; 
  int i;

  if (count > (sizeof(fields)) / (sizeof(val_t)))
    BUG("alloc_manyint not quite fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;
  for (i=0; i<(sizeof(masks)) / (sizeof(int)); i++)
    masks[i] = 0;

  return alloc_record(fields, masks, count);
}

ptr_t alloc_manyintrec(int count, int v, ptr_t rec)
{
  int masks[100/RECLEN_MAX];
  val_t fields[100];
  int i;

  if (count>100)
    BUG("allocating record too large");

  for (i=0; i<100/RECLEN_MAX; i++)
    masks[i] = 0;
  masks[count/RECLEN_MAX] |= 1 << (count % RECLEN_MAX);
  
  for (i=0; i<count; i++)
    fields[i] = v;
  fields[count] = (val_t) rec;
  
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
  int tag = IARRAY_TAG | (byteLen << ARRLEN_OFFSET);
  obj[-1] = tag;
  for (i=0; i<(byteLen + 3) / 4; i++)
    obj[i] = v;
}

void init_parray(ptr_t obj, int len, ptr_t v)
{
  int i;
  int byteLen = 4 * len;
  int tag = PARRAY_TAG | (byteLen << ARRLEN_OFFSET);
  obj[-1] = tag;
  for (i=0; i<len; i++)
    obj[i] = (val_t) v;
}

void init_farray(ptr_t obj, int len, double v)
{
  int i;
  int byteLen = 8 * len;
  int tag = RARRAY_TAG | (byteLen << ARRLEN_OFFSET);
  assert((((unsigned int)obj) & 7) == 0);
  obj[-1] = tag;
  for (i=0; i<len; i++)
    ((double *)obj)[i] = v;
}

