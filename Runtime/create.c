#include "tag.h"
#include "general.h"
#include <assert.h>

value_t alloc_manyintrec(int count, int v, value_t rec,
			 value_t *alloc_ptr, value_t limit);

value_t alloc_iarray(int count, int n,
		     value_t *alloc_ptr, value_t limit)
{
  value_t obj = 0;
  int len = count;
  int mask = 0;

#ifdef HEAPPROFILE
  *(value_t *)(*alloc_ptr) = 30000;
  (*alloc_ptr) += 4;
#endif
  obj = (*alloc_ptr) + 4;
  if (len == 0) len++;
  if (*alloc_ptr + 4*(len+1) > limit)
    BUG("OUT OF SPACE");
  (*alloc_ptr) += 4*(len+1);

  {
    int *robj = (int *)obj;
    robj[-1] = IARRAY_TAG | (count << (2+POSSLEN_SHIFT));
    while (len > 0)
      robj[--len]  = n;
  }
  return obj;
}

void oddword_align(value_t *alloc_ptr)
{
  int v = (int) (*alloc_ptr);
  if ((v & 7) == 0)
    {
      *((int *)v) = SKIP_TAG;
      *alloc_ptr = (value_t)(v+4);
    }
}

void evenword_align(value_t *alloc_ptr)
{
  int v = (int) (*alloc_ptr);
  if ((v & 7) != 0)
    {
      *((int *)v) = SKIP_TAG;
      *alloc_ptr = (value_t)(v+4);
    }
}

value_t alloc_rarray(int count, double val,
		     value_t *alloc_ptr, value_t limit)
{
  int len = 2 * count; /* in 4-byte words */
  int mask = 0;
  value_t obj;

#ifdef HEAPPROFILE
  evenword_align(alloc_ptr);
  *(value_t *)(*alloc_ptr) = 30001;
  (*alloc_ptr) += 4;
#else
  oddword_align(alloc_ptr);
#endif

  obj = (*alloc_ptr) + 4;
  if (len == 0) len++;
  if (*alloc_ptr + 4*(len+1) > limit)
    BUG("OUT OF SPACE");
  (*alloc_ptr) += 4*(len+1);

  {
    int *robj = (int *)obj;
    robj[-1] = RARRAY_TAG | (len << (2+POSSLEN_SHIFT));
    while (count > 0)
      {
	count--;
	((double *)robj)[count]  = val;
      }
  }
  return obj;
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
      int len = (olen > MAX_RECORDLEN) ? MAX_RECORDLEN : olen;
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

value_t alloc_record(value_t *alloc_ptr, value_t limit,
		     value_t *fields, int *masks, int orig_count)
{
  int count = (orig_count < 1) ? 1 : orig_count;
  int temp = (count + MAX_RECORDLEN - 1) / MAX_RECORDLEN;
  int numtag = (temp < 1) ? 1 : temp;
  value_t *tagstart, *objstart;
  int subpart = 0;

#ifdef HEAPPROFILE
  *(value_t *)(*alloc_ptr) = 30002;
  (*alloc_ptr) += 4;
#endif

  tagstart = (value_t *)(*alloc_ptr);
  objstart = (value_t *)((*alloc_ptr) + 4 * numtag);


  if (*alloc_ptr + 4*(numtag+count) > limit)
    BUG("OUT OF SPACE");
  (*alloc_ptr) += 4*(numtag+count);

  while (count > 0)
    {
      int i,j;
      int temp = (count<=MAX_RECORDLEN) ? count : MAX_RECORDLEN;
      int len = (temp < 1) ? 1 : temp;
      int masklen = (count<=MAX_RECORDLEN) ? count : (MAX_RECORDLEN+1);
      int mask = 0;
      int tag = (subpart==0) ? RECORD_TAG : RECORD_SUB_TAG;
      for (i=0; i<len; i++)
	{
	  int logbit = subpart*MAX_RECORDLEN + i;
	  int whichbyte = logbit >> 5;
	  int whichbit = logbit & 31;
	  int ison = masks[whichbyte] & (1 << whichbit);
	  if (ison)
	    mask |= 1 << i;
	}
      tag = tag | (masklen << 27) | (mask << 3);
      tagstart[subpart] = tag;
      
      for (i=0; i<len; i++)
	objstart[i+subpart*MAX_RECORDLEN] = fields[subpart*MAX_RECORDLEN+i];
      
      count -= len;
      subpart++;
    }

  return (value_t)objstart;
}

value_t alloc_intrec(int n, value_t rec,
		       value_t *alloc_ptr, value_t limit)
{
  return alloc_manyintrec(1,n,rec,alloc_ptr,limit);
}

value_t alloc_string(int strlen, char *str,
		       value_t *alloc_ptr, value_t limit)
{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  value_t res = 0; 
  int tag = IARRAY_TAG | (strlen << POSSLEN_SHIFT);
#ifdef HEAPPROFILE
  *(value_t *)(*alloc_ptr) = 30003;
  (*alloc_ptr) += 4;
#endif
  res = (*alloc_ptr) + 4;
  if (*alloc_ptr + 4*(wordlen+1) > limit)
    BUG("OUT OF SPACE");
  (*alloc_ptr) += 4*((wordlen?wordlen:1)+1);

  ((int *)res)[-1] = tag;
  bcopy(str,(char *)res,strlen);

  return res;
}


value_t alloc_uninit_string(int strlen, char **raw,
			    value_t *alloc_ptr, value_t limit)
{
  int offset = 0;
  int wordlen = (strlen + 3) / 4;
  value_t res;
  int tag = IARRAY_TAG | (strlen << POSSLEN_SHIFT);
#ifdef HEAPPROFILE
  *(value_t *)(*alloc_ptr) = 30004;
  (*alloc_ptr) += 4;
#endif
  res = (*alloc_ptr) + 4;
  if (*alloc_ptr + 4*(wordlen+1) > limit)
    BUG("OUT OF SPACE");
  (*alloc_ptr) += 4*(wordlen+1);

  ((int *)res)[-1] = tag;
  *raw = (char *)res;
  return res;
}

/* this is dangerous .... */
void adjust_stringlen(value_t str, int newlen)
{
  int newtag = IARRAY_TAG | (newlen << POSSLEN_SHIFT);
  value_t *obj = (value_t *)str;
  obj[-1] = newtag;
}


value_t alloc_recrec(value_t rec1, value_t rec2,
		     value_t *alloc_ptr, value_t limit)
{
  value_t fields[2];
  int mask = 3;
  fields[0] = rec1;
  fields[1] = rec2;

  return  alloc_record(alloc_ptr, limit, fields, &mask, 2);
}


value_t alloc_manyint(int count, int v,
		      value_t *alloc_ptr, value_t limit)
{
  static int masks[100/MAX_RECORDLEN];
  static int firsttime = 1;
  value_t fields[100]; /* making this static is thread-unsafe */
  int i;

  if (firsttime)
    {
      for (i=0; i<100/MAX_RECORDLEN; i++)
	masks[i] = 0;
      firsttime = 0;
    }

  if (count>100)
    BUG("alloc_manyint not quite fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;

  return  alloc_record(alloc_ptr, limit, fields, masks, count);
}

value_t alloc_manyintrec(int count, int v, value_t rec,
			 value_t *alloc_ptr, value_t limit)
{
  int masks[100/MAX_RECORDLEN];
  value_t fields[100]; /* making this static is thread-unsafe */
  int i;

  if (count>100)
    BUG("allocating record too large");

  for (i=0; i<100/MAX_RECORDLEN; i++)
    masks[i] = 0;
  masks[count/MAX_RECORDLEN] |= 1 << (count % MAX_RECORDLEN);
  
  for (i=0; i<count; i++)
    fields[i] = v;
  fields[count] = rec;
  
  return  alloc_record(alloc_ptr, limit, fields, masks, count+1);
}
