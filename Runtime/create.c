#include "general.h"
#include <string.h>
#include "tag.h"
#include "thread.h"
#include "global.h"
#include "create.h"
#include "gc.h"

const value_t empty_record = 256; /* This is the ML unit. */

value_t* oddword_align(value_t *ptr)
{
  int v = (int) ptr;
  if ((v & 7) == 0)
    {
      *ptr = SKIP_TAG;
      return ptr + 1;
    }
  return ptr;
}

value_t* evenword_align(value_t *ptr)
{
  int v = (int) ptr;
  if ((v & 7) != 0)
    {
      *ptr = SKIP_TAG;
      return ptr + 1;
    }
  return ptr;
}

static value_t* alloc_space(int bytesNeeded) 
{
  value_t alloc, limit, newAlloc;
  Thread_t *th = getThread();
  if (th != NULL) {
    alloc = th->saveregs[ALLOCPTR];
    limit = th->saveregs[ALLOCLIMIT];
    newAlloc = alloc + bytesNeeded;
    if (newAlloc <= limit) {
      th->saveregs[ALLOCPTR] = newAlloc;
      return (value_t *) alloc;
    }
    else {
      GCFromC(th,bytesNeeded,0);
      alloc = th->saveregs[ALLOCPTR];
      limit = th->saveregs[ALLOCLIMIT];
      newAlloc = alloc + bytesNeeded;
      assert(newAlloc <= limit);  /* This time the request must have been satisfied. */
      th->saveregs[ALLOCPTR] = newAlloc;
      return (value_t *) alloc;
    }
  }
  else {
    alloc = (value_t)RuntimeGlobalData_Cur;
    limit = (value_t)RuntimeGlobalData_End;
    newAlloc = alloc + bytesNeeded;
    assert(newAlloc <= limit);  /* We should not allocate from global segment too much. */
    RuntimeGlobalData_Cur = newAlloc;
    return (value_t *) alloc;
  }
}


value_t alloc_iarray(int count, int n)

{
  value_t *obj = 0;
  int len = count ? count : 1;
  int mask = 0;
  value_t *alloc = alloc_space(4 * (len + 1));

  obj = alloc + 1;
  obj[-1] = IARRAY_TAG | (count << (2+ARRLEN_OFFSET));
  while (len > 0)
    obj[--len]  = n;
  
  return (value_t) obj;
}

value_t alloc_rarray(int count, double val)

{
  int len = count ? count : 1;
  int mask = 0;
  value_t *obj;

  value_t *alloc = alloc_space(8 * (len + 1)); /* tags and alignment */
  alloc = oddword_align(alloc);
  obj = alloc + 1;
  obj[-1] = RARRAY_TAG | (len << (2+ARRLEN_OFFSET));
  while (count > 0)
    {
      count--;
      ((double *)obj)[count]  = val;
    }
  obj[2 * len] = SKIP_TAG;
  
  return (value_t) obj;
}

value_t get_record(value_t rec, int which)
{
  value_t *objstart = (value_t *) rec;
  value_t tag = objstart[-1];
  int len = GET_RECLEN(tag);

  if (!(IS_RECORD(tag))) {
    printf("BUG: calling get_field on non-record. tag = %d\n",tag);
    exit(-1);
  }

  if (which < len)
    return objstart[which];
  else
    {
      printf("BUG: calling get_field with"
	     " non-existent field %d on rec of len %d at addr %d\n",
	     which,len,objstart);
      exit(-1);
    }
}

value_t alloc_record(value_t *fields, int *masks, int count)
{
  int i;
  value_t *objstart;
  value_t *alloc;
  assert(count <= RECLEN_MAX);
  if (count == 0)
    return empty_record;

  alloc = alloc_space(4 * (count + 1));
  objstart = alloc + 1;

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
    objstart[-1] = tag;
  }

  /* Initialize record fields */
  for (i=0; i<count; i++)
    objstart[i] = fields[i];
      
  return (value_t)objstart;
}



value_t alloc_string(int strlen, char *str)

{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  value_t *res;
  int tag = IARRAY_TAG | (strlen << ARRLEN_OFFSET);

  value_t *alloc = alloc_space(4 * (wordlen + 1));
  res = alloc + 1;
  res[-1] = tag;
  bcopy(str,(char *)res,strlen);

  return (value_t) res;
}


value_t alloc_uninit_string(int strlen, char **raw)
{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  value_t *res;
  int tag = IARRAY_TAG | (strlen << ARRLEN_OFFSET);

  value_t *alloc = alloc_space(4 * (wordlen + 1));
  res = alloc + 1;
  res[-1] = tag;
  *raw = (char *)res;
  return (value_t)res;
}

/* Shorten a string and fill the space it used to occupy with SKIP_TAG */
void adjust_stringlen(value_t str, int newlen)
{
  value_t *obj = (value_t *)str;
  int oldtag = obj[-1];
  int newtag = IARRAY_TAG | (newlen << ARRLEN_OFFSET);
  int oldlen = GET_ARRLEN(oldtag);
  int oldword = (oldlen + 3) / 4;
  int newword = (newlen + 3) / 4;
  int i;
  assert(newlen <= oldlen);
  obj[-1] = newtag;
  for (i=newword+1; i<=oldword; i++)
    obj[i] = SKIP_TAG;
}


value_t alloc_recrec(value_t rec1, value_t rec2)
{
  value_t fields[2];
  int mask = 3;
  fields[0] = rec1;
  fields[1] = rec2;

  return alloc_record(fields, &mask, 2);
}


value_t alloc_manyint(int count, int v)
{
  int masks[1 + (100/RECLEN_MAX)];
  value_t fields[100]; 
  int i;

  if (count > (sizeof(fields)) / (sizeof(value_t)))
    BUG("alloc_manyint not quite fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;
  for (i=0; i<(sizeof(masks)) / (sizeof(int)); i++)
    masks[i] = 0;

  return alloc_record(fields, masks, count);
}

value_t alloc_manyintrec(int count, int v, value_t rec)

{
  int masks[100/RECLEN_MAX];
  value_t fields[100];
  int i;

  if (count>100)
    BUG("allocating record too large");

  for (i=0; i<100/RECLEN_MAX; i++)
    masks[i] = 0;
  masks[count/RECLEN_MAX] |= 1 << (count % RECLEN_MAX);
  
  for (i=0; i<count; i++)
    fields[i] = v;
  fields[count] = rec;
  
  return alloc_record(fields, masks, count+1);
}

value_t alloc_intrec(int n, value_t rec)
{
  return alloc_manyintrec(1,n,rec);
}

value_t alloc_intint(int a, int b)
{
  value_t result = alloc_manyint(2,a);
  ((int *)result)[1] = b;
  return result;
}
