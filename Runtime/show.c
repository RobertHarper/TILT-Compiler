#include <stdlib.h>
#include <assert.h>
#include "general.h"
#include "tag.h"
#include "memobj.h"
#include "show.h"



/* Check that pointer is either a tag, a global, or a current heap value */
int ptr_check(value_t *loc, Heap_t **legalHeaps)
{
  value_t pointer = *loc;
  if (IsConstructorData(pointer))
    return 1;
  if (IsGlobalData(pointer))
    return 1;
  if (pointer == 258) /* Uninitialized pointer */
    return 1;
  while (1) {
    Heap_t *curHeap = *(legalHeaps++);
    if (curHeap == NULL)
      break;
    if (pointer >= curHeap->bottom && pointer < curHeap->top)
      return 1;
  }
  printf("TRACE ERROR: ptr_check %d at %d failed.  GC #%d\n",pointer,loc,NumGC);
  return 0;
}

extern int heapstart;

/* If i looks like a printer, issue a warning. */
int data_check(value_t *loc, Heap_t **legalHeaps)
{
  value_t i = *loc;
  if (verbose) {
    while (1) {
      Heap_t *curHeap = *(legalHeaps++);
      if (curHeap == NULL)
	break;
      if (i >= curHeap->bottom && i < curHeap->alloc_start) 
	printf("TRACE WARNING: data_check given int that looks like a curHeap pointer %d\n", i);
    }
   if (i > heapstart)
     printf("TRACE WARNING: data_check given int that is large enough to be pointer %d\n", i);
  }
  return 1;
}

value_t show_obj(value_t s, int checkonly, Heap_t **legalHeaps)
{
  value_t *start = (value_t *)s, *end = NULL;
  value_t *obj = start + 1;
  int i, tag, type;
#ifdef HEAPPROFILE
  assert(0);
#endif

  tag = start[0];
  type = GET_TYPE(tag);
  switch (type)
    {
    case RECORD_TAG:
      {
	int i;
	int len = GET_RECLEN(tag);
	int mask = GET_RECMASK(tag);
	if (mask >> len) {
	  printf("TRACE ERROR: Bad record tag %d at %d\n", tag, start);
	  assert(0);
	}
	if (!checkonly)
	  printf("%ld: REC(%d)    ", obj,len);
	for (i=0; i<len; i++) {
	  value_t field = obj[i];
	  int isPointer = 1 & (mask >> i);
	  if (isPointer) {
	    if (!checkonly)
	      printf("P(%5d)  ",field);
	    ptr_check(obj+i,legalHeaps);
	  }
	  else {
	    if (!checkonly)
	      printf("I(%5d)  ",field);
	    data_check(obj+i,legalHeaps);
	  }
	}
	if (!checkonly)
	  printf("\n");
	end = obj + len;
      }
      break;
    case FORWARD_TAG:
      {
	value_t *forwardstart = ((value_t *)start[0]) - 1;
	value_t *forwardend = NULL;
	int len;

	if (!checkonly)
	  printf("%ld: SENT(%d) -> ",obj,forwardstart + 1);
	forwardend = (value_t *)(show_obj((value_t)forwardstart,checkonly,legalHeaps));
	len = forwardend - forwardstart;
	end = start + len;
      }
      break;
    case IARRAY_TAG:
    case PARRAY_TAG:
    case RARRAY_TAG:
      {
	unsigned int bytelen = GET_ARRLEN(tag);
	unsigned int wordlen = (bytelen + 3) / 4;
	unsigned int loglen;

	if (type == IARRAY_TAG) {
	  loglen = bytelen;
	  if (!checkonly)
	    printf("%ld: IAR(%ld)  ",start,wordlen,bytelen);
	}
	else if (type == RARRAY_TAG) {
	  loglen = bytelen / 8;
	  if (!checkonly)
	    printf("%ld: RAR(%ld/%ld)  ",start,wordlen,loglen);
	}
	else if (type == PARRAY_TAG) {
	  loglen = bytelen / 4;
	  if (!checkonly)
	    printf("%ld: PAR(%ld/%ld)  ",start,wordlen,loglen);
	}
	else 
	  assert(0);

	for (i=0; i<((type==IARRAY_TAG)?wordlen:loglen); i++) {
	  if (!checkonly && ((i) / 8 * 8) == (i) && (i != 0))
	    printf("        ");
	  switch (type) {
	    case IARRAY_TAG: {
	      value_t field = obj[i];
	      if (!checkonly)
		printf("I(%ld)  ",field);
	      data_check(obj+i,legalHeaps);
	      break;
	    }
	    case PARRAY_TAG: {
	      value_t field = obj[i];
	      if (!checkonly)
		printf("%ld   ",field);
	      ptr_check(obj+i,legalHeaps);
	      break;
	    }
	    case RARRAY_TAG: {
	      if (!checkonly)
		printf("%lf   ",((double *)obj)[i]);
	      break;
	    }
	  default : 
	    assert(0);
	  }
	  if (!checkonly && ((i+1) / 8 * 8) == (i+1))
	    printf("\n");
	}
	if (wordlen == 0)
	  end = obj + 1;
	else 
	  end = obj + wordlen;
      }
      break;
    case SKIP_TAG:
      if (!checkonly)
	printf("%ld: SKIP\n", start);
      end = obj;
      break;
    default:
      printf("\nshow.c:show_obj  tag = %d(%d) at address = %d\b",tag,GET_TYPE(tag),s);
      assert(0);
      break;
    }
  return (value_t)end;
}




void scan_heap(char *label, value_t start, value_t finish, value_t top, Heap_t **legalHeaps, int show)
{
  if (NumGC < LEAST_GC_TO_CHECK)
    return;
  if (show) {
    printf("--------------HEAP CHECK START GC %d ------------------------------\n",NumGC);
    printf("%s %d <= %d < %d\n",label,start,finish,top);
    printf("--------------------------------------------------------------\n");
  }
  while (start < finish)
      start = show_obj(start,!show,legalHeaps);
  if (show) {
    printf("--------------HEAP CHECK END-----------------------------------\n\n");
  }
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
      start = show_obj(start,1,NULL);
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


