#include "general.h"
#include <string.h>
#include "tag.h"
#include "thread.h"
#include "global.h"

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

value_t alloc_manyintrec(int count, int v, value_t rec);

void get_alloc_limit(value_t **alloc, value_t **limit)
{
  Thread_t *th = getThread();
  if (th != NULL)
    {
      *alloc = (value_t *)(th->saveregs[ALLOCPTR]);
      *limit = (value_t *)(th->saveregs[ALLOCLIMIT]);
    }
  else
    {
      *alloc = (value_t *)RuntimeGlobalData_Cur;
      *limit = (value_t *)RuntimeGlobalData_End;
    }
}

void set_alloc(value_t *alloc)
{
  Thread_t *th = getThread();
  if (th != NULL)
      th->saveregs[ALLOCPTR] = (long) alloc;
  else
      RuntimeGlobalData_Cur = (value_t) alloc;
}

value_t alloc_iarray(int count, int n)

{
  value_t *obj = 0;
  int len = count;
  int mask = 0;

  value_t *alloc, *limit;
  get_alloc_limit(&alloc,&limit);

#ifdef HEAPPROFILE
  *alloc = 30000;
  alloc++;
#endif
  obj = alloc + 1;
  if (len == 0) len++;
  alloc += len+1;
  if (alloc > limit) 
    { assert(0); }  /* OUT OF SPACE */

  set_alloc(alloc);

  obj[-1] = IARRAY_TAG | (count << (2+ARRLEN_OFFSET));
  while (len > 0)
    obj[--len]  = n;
  
  return (value_t) obj;
}

value_t alloc_rarray(int count, double val)

{
  int len = 2 * count; /* in 4-byte words */
  int mask = 0;
  value_t *obj;

  value_t *alloc, *limit;
  get_alloc_limit(&alloc,&limit);

#ifdef HEAPPROFILE
  alloc = evenword_align(alloc);
  *alloc = 30001;
  alloc++;
#else
  alloc = oddword_align(alloc);
#endif

  obj = alloc + 1;
  if (len == 0) len++;
  alloc += (len+1);
  if (alloc > limit)
    { assert(0); }  /* OUT OF SPACE */

  set_alloc(alloc);

  obj[-1] = RARRAY_TAG | (len << (2+ARRLEN_OFFSET));
  while (count > 0)
    {
      count--;
      ((double *)obj)[count]  = val;
    }
  
  return (value_t) obj;
}

value_t get_record(value_t rec, int which)
{
  value_t *tagstart = ((value_t *) rec) - 1;
  value_t *objstart = (value_t *) rec;
  int fieldlen = 0;

  while (1)
    {
      int tag = tagstart[0];
      int olen = GET_RECLEN(tag);
      int len = (olen > RECLEN_MAX) ? RECLEN_MAX : olen;
      if (IS_RECORD(tag))
	{
	  fieldlen += len;
	  break;
	}
      else if (IS_RECORD_SUB(tag))
	{
	  fieldlen += len;
	}
      else 
	{
	  printf("BUG: calling get_field on non-record\n");
	  exit(-1);
	}
      tagstart--;
    }


  if (which < fieldlen)
    return objstart[which];
  else
    {
      printf("BUG: calling get_field with"
	     " non-existent field %d on rec of len %d at addr %d\n",
	     which,fieldlen,objstart);
      exit(-1);
    }
}

value_t alloc_record(value_t *fields, int *masks, int orig_count)
{
  int count = (orig_count < 1) ? 1 : orig_count;
  int temp = (count + RECLEN_MAX - 1) / RECLEN_MAX;
  int numtag = (temp < 1) ? 1 : temp;
  value_t *tagstart, *objstart;
  int subpart = 0;

  value_t *alloc, *limit;
  get_alloc_limit(&alloc,&limit);

#ifdef HEAPPROFILE
  *alloc = 30002;
  alloc++;
#endif

  tagstart = alloc;
  objstart = alloc + numtag;

  alloc += numtag + count;
  if (alloc > limit)
    { assert(0); }  /* OUT OF SPACE */

  set_alloc(alloc);
  
  while (count > 0)
    {
      int i,j;
      int temp = (count<=RECLEN_MAX) ? count : RECLEN_MAX;
      int len = (temp < 1) ? 1 : temp;
      int masklen = (count<=RECLEN_MAX) ? count : (RECLEN_MAX+1);
      int mask = 0;
      int tag = (subpart==0) ? RECORD_TAG : RECORD_SUB_TAG;
      for (i=0; i<len; i++)
	{
	  int logbit = subpart*RECLEN_MAX + i;
	  int whichbyte = logbit >> 5;
	  int whichbit = logbit & 31;
	  int ison = masks[whichbyte] & (1 << whichbit);
	  if (ison)
	    mask |= 1 << i;
	}
      tag = tag | (masklen << RECLEN_OFFSET) | (mask << RECMASK_OFFSET);
      tagstart[subpart] = tag;
      
      for (i=0; i<len; i++)
	objstart[i+subpart*RECLEN_MAX] = fields[subpart*RECLEN_MAX+i];
      
      count -= len;
      subpart++;
    }

  return (value_t)objstart;
}

value_t alloc_intrec(int n, value_t rec)
{
  return alloc_manyintrec(1,n,rec);
}

value_t alloc_string(int strlen, char *str)

{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  value_t *res;
  int tag = IARRAY_TAG | (strlen << ARRLEN_OFFSET);

  value_t *alloc, *limit;
  get_alloc_limit(&alloc,&limit);

#ifdef HEAPPROFILE
  *alloc = 30003;
  alloc++;
#endif
  res = alloc + 1;
  alloc += (wordlen?wordlen:1)+1;
  if (alloc > limit)
    { assert(0); }  /* OUT OF SPACE */

  set_alloc(alloc);

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

  value_t *alloc, *limit;
  get_alloc_limit(&alloc,&limit);

#ifdef HEAPPROFILE
  *alloc = 30004;
  alloc++;
#endif
  res = alloc + 1;
  alloc += wordlen+1;
  if (alloc > limit)
    { assert(0); }  /* OUT OF SPACE */

  set_alloc(alloc);

  res[-1] = tag;
  *raw = (char *)res;
  return (value_t)res;
}

/* this is dangerous .... */
void adjust_stringlen(value_t str, int newlen)
{
  int newtag = IARRAY_TAG | (newlen << ARRLEN_OFFSET);
  value_t *obj = (value_t *)str;
  obj[-1] = newtag;
}


value_t alloc_recrec(value_t rec1, value_t rec2)
{
  value_t fields[2];
  int mask = 3;
  fields[0] = rec1;
  fields[1] = rec2;

  return  alloc_record(fields, &mask, 2);
}


value_t alloc_manyint(int count, int v)

{
  static int masks[100/RECLEN_MAX];
  static int firsttime = 1;
  value_t fields[100]; /* making this static is thread-unsafe */
  int i;

  if (firsttime)
    {
      for (i=0; i<100/RECLEN_MAX; i++)
	masks[i] = 0;
      firsttime = 0;
    }

  if (count>100)
    BUG("alloc_manyint not quite fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;

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
