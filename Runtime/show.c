#include <stdlib.h>
#include <assert.h>
#include "tag.h"
#include "show.h"
#include "general.h"
#include "memobj.h"


int ptr_check(value_t unused)
{
  assert(0);
}

value_t show_obj(value_t s, int checkonly)
{
  value_t *start = NULL;
  int i, tag;
#ifdef HEAPPROFILE
  int proftag = *((int *)(s));
  if ((*(value_t *)s) != SKIP_TAG)
    s += 4;
#endif

  start = (value_t *)s;
  tag = start[0];
  switch (GET_TYPE(tag))
    {
    case RECORD_TAG:
      {
	value_t *rawstart = start;
	value_t *objstart = start;
	int fieldlen = 0, i;
	while (1)
	  {
	    int curlen = GET_RECLEN(*objstart);
	    if (!(IS_RECORD(*objstart) ||
		  (IS_RECORD_SUB(*objstart))))
	      assert(0);
	    fieldlen += (curlen > RECLEN_MAX) ? RECLEN_MAX : curlen;
	    objstart++;
	    if (curlen <= RECLEN_MAX)
	      break;
	  }
#ifdef HEAPPROFILE
	if (!checkonly)
	  printf("%ld(%d): REC(%d)    ", objstart,proftag, fieldlen);
#else
	if (!checkonly)
	  printf("%ld: REC(%d)    ", objstart,fieldlen);
#endif
	if (fieldlen == 0)
	  {
	    if (!checkonly)
	      printf("NONE\n");
	    start += 2;
	    break;
	  }	  
	for (i=0; i<fieldlen; i++)
	  {
	    unsigned int mask = GET_RECMASK(rawstart[i / RECLEN_MAX]);
	    int trace = mask & (1 << i % RECLEN_MAX);
	    if (trace)
	      {
		if (!checkonly)
		  printf("%5d  ",objstart[i]);
		ptr_check(objstart[i]);
	      }
	    else
	      if (!checkonly)
		printf("I(%4d)  ",objstart[i]);
	  }
	if (!checkonly)
	  printf("\n");
	start = objstart + fieldlen;
      }
      break;
    case FORWARD_TAG:
      {
	value_t curpos,nextpos;
	start++;
	curpos = start[0] - 4;
/* scan back to beginning in case of record */
	while (IS_RECORD_SUB(*((int *)curpos)))
	  curpos -= 4;
#ifdef HEAPPROFILE
	curpos -= 4;
#endif
#ifdef HEAPPROFILE
	if (!checkonly)
	  printf("%ld(%d): SENT(%d) -> ",start,proftag,curpos);
#else
	if (!checkonly)
	  printf("%ld: SENT(%d) -> ",start,curpos);
#endif
	nextpos = show_obj(curpos,checkonly);
	start += (nextpos - start[0])/4;
      }
      break;
    case IARRAY_TAG:
    case PARRAY_TAG:
    case RARRAY_TAG:
      {
	unsigned int len = (GET_ARRLEN(start[0]) + 3) / 4;
	unsigned int type = GET_TYPE(tag);
	start++;
	if (!checkonly)
	  {
#ifdef HEAPPROFILE
	    if (type == IARRAY_TAG)
	      printf("%ld(%d): IAR(%ld)  ",start,proftag,len);
	    else if (type == RARRAY_TAG)
	      printf("%ld(%d): RAR(%ld/%ld)  ",start,proftag,len,len/2);
	    else 
	      printf("%ld(%d): PAR(%ld)  ",start,proftag,len);
#else
	    if (type == IARRAY_TAG)
	      printf("%ld: IAR(%ld)  ",start,len);
	    else if (type == RARRAY_TAG)
	      printf("%ld: RAR(%ld/%ld)  ",start,len,len/2);
	    else 
	      printf("%ld: PAR(%ld)  ",start,len);
#endif
	  }
	if (len > 0)
	  {
	    int loglen = (type == RARRAY_TAG) ? len/2 : len;
	    for (i=0; i<loglen && i<40 ; i++)
	      switch (type)
		{
		case IARRAY_TAG:
		  if (!checkonly)
		    printf("I(%ld)  ",start[i]);
		  break;
		case PARRAY_TAG:
		  if (!checkonly)
		    printf("%ld     ",start[i]);
		  ptr_check(start[i]);
		  break;
		case RARRAY_TAG:
		  if (!checkonly)
		    printf("%lf     ",((double *)start)[i]);
		  break;
		}
	    if (!checkonly)
	      {
		if (loglen > 40) printf (" ... ");
		printf("\n");
	      }
	    start += len;
	  }
	else
	  {
	    if (!checkonly)
	      printf("NONE\n");
	    start += 1;
	  }
      }
      break;
    case SKIP_TAG:
      if (!checkonly)
	printf("%ld: SKIP\n", start);
      return (value_t)(start+1);
    case RECORD_SUB_TAG:
    default:
      printf("\ntag = %d(%d)  at add = %d",tag,GET_TYPE(tag),s);
      BUG("IMPOSSIBLE\n");
      return (value_t)0;
    }
  return (value_t)start;
}


void show_heap(char *label, value_t start, value_t finish, value_t top)
{
  if (NumGC < LEAST_GC_TO_CHECK)
    return;
  printf("--------------HEAP SHOW START GC %d ------------------------------\n",NumGC);
  printf("%s %d <= %d < %d\n",label,start,finish,top);
  printf("--------------------------------------------------------------\n");
  while (start < finish)
      start = show_obj(start,0);
  printf("--------------HEAP SHOW END-----------------------------------\n\n");
}


void check_heap(char *label, value_t start, value_t finish, value_t top)
{
  if (NumGC < LEAST_GC_TO_CHECK)
    return;
  printf("--------------HEAP CHECK START GC %d ------------------------------\n",NumGC);
  printf("%s %d <= %d < %d\n",label,start,finish,top);
  printf("--------------------------------------------------------------\n");
  while (start < finish)
      start = show_obj(start,1);
  printf("--------------HEAP CHECK END-----------------------------------\n\n");
}

#ifdef SEMANTIC_GARBAGE  
extern unsigned long semantic_garbage_offset;
extern int NumGC, NumMajorGC;
unsigned long SemanticGarbageSize = 0;

void scandead_heap(char *label, value_t start, value_t finish, value_t top)
{
  unsigned long temp = 0, temp2 = 0;
  printf("\n\n");
  printf("--------------SCANDEAD HEAP START --------------------------------\n");
  printf("%s %d <= %d < %d\n",label,start,finish,top);
  printf("------------------------------------------------------------------\n");
  while (start < finish)
    {
      value_t *cur = (value_t *)start;
      value_t tag = *cur;
      start = show_obj(start,1);
      if (tag != FORWARD_TAG)
	{
	  int size = start - (value_t)cur;
	  value_t stamp_add = ((value_t)cur) + semantic_garbage_offset;
	  int stamp = *((int *)stamp_add);
	  temp += size*(NumMajorGC-stamp);
	  temp2 += size;
	  /*	  printf("Dead obj: size = %d    life = %d  loc = %d\n",size,NumMajorGC-stamp,cur); */
	}
    }
  printf("GC = %d,  sz*life = %d  sz = %d\n",NumMajorGC,temp,temp2);
  SemanticGarbageSize += temp;
  printf("--------------SCANDEAD HEAP END-----------------------------------\n\n");
}
#endif

void show_heap_raw(char *label, int numwords,
		   value_t from_low, value_t from_high,
		   value_t to_low,   value_t to_high)
{
  int f;
  printf("---------HEAP SHOW RAW BEGIN : %s   ----------\n",label);	
  for (f=0; f<4*numwords; f+=4)
    printf("%ld: %ld       %ld: %ld\n",
	   from_low+f,*((int *)(from_low+f)),
	   to_low+f,*((int *)(to_low+f)));
  printf("---------HEAP SHOW RAW BEGIN ------------------------\n");	
}

void memdump(char *title, int *start, int len, int *target)
{
  int i;
  if (NumGC < LEAST_GC_TO_CHECK)
    return;
  printf("Memory dump start: %s\n",title);
  for (i=0; i<len; i++)
    {
      int *addr = start + i;
      printf("  %u: %u",addr,*addr);
      if (addr == target)
	printf("     <---------");
      printf("\n");
    }
  printf("Memory dump end: %s\n",title);
}


