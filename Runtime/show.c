#include <stdlib.h>
#include <assert.h>
#include "general.h"
#include "tag.h"
#include "memobj.h"
#include "show.h"
#include "global.h"



int inHeaps(ptr_t v, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  while (1) {
    Heap_t *curHeap = *(legalHeaps++);
    Bitmap_t *curStart = (legalStarts == NULL) ? NULL : *(legalStarts++);
    if (curHeap == NULL)
      break;
    if ((mem_t) v >= curHeap->bottom && (mem_t) v < curHeap->top) {
      unsigned int wordOffset = ((mem_t) v) - curHeap->bottom;
      if (curStart == NULL)
	return 1;
      if (!(IsSet(curStart, wordOffset)))
	printf("!!! wordOffset %d not set\n",wordOffset);
      return IsSet(curStart, wordOffset);
    }
  }
  return 0;
}

/* Check that pointer is either a tag, a global, or a current heap value */
int ptr_check(ptr_t *ptrLoc, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  ptr_t pointer = *ptrLoc;
  if (IsTagData(pointer))
    return 1;
  if (IsGlobalData(pointer))
    return 1;
  if (pointer == (ptr_t) 258) /* Uninitialized pointer */
    return 1;
  if (inHeaps(pointer,legalHeaps,legalStarts))
    return 1;
  printf("\n  !!!!TRACE ERROR: ptr_check %d at %d failed.  GC #%d\n",pointer,ptrLoc,NumGC);
  return 0;
}

extern int heapstart;

/* If it looks like a pointer, issue a warning. */
int data_check(val_t *loc, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  val_t i = *loc;
  if (verbose) 
    if (inHeaps((ptr_t) i,legalHeaps,legalStarts))
      printf("TRACE WARNING: data_check given int that looks like a curHeap pointer %d\n", i);
  return 1;
}

/* Show the object whose raw beginning (i.e. including tag(s)) is at s */
mem_t show_obj(mem_t start, int show, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  mem_t end = NULL;
  ptr_t obj = start + 1;
  tag_t tag = start[0];
  int type = GET_TYPE(tag);
  int i;

  switch (type)
    {
    case RECORD_TAG:
      {
	int len = GET_RECLEN(tag);
	int mask = GET_RECMASK(tag);
	if (mask >> len) {
	  printf("TRACE ERROR: Bad record tag %d at %d\n", tag, start);
	  assert(0);
	}
	if (show)
	  printf("%ld: REC(%d)    ", obj,len);
	for (i=0; i<len; i++) {
	  val_t field = obj[i];
	  int isPointer = 1 & (mask >> i);
	  if (isPointer) {
	    if (show)
	      printf("P(%5d)  ",field);
	    ptr_check((ptr_t *)(obj+i),legalHeaps,legalStarts);
	  }
	  else {
	    if (show)
	      printf("I(%5d)  ",field);
	    data_check(obj+i,legalHeaps,legalStarts);
	  }
	}
	if (show)
	  printf("\n");
	end = obj + len;
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
	  if (show)
	    printf("%ld: IAR(%ld/%ld)  ",obj,wordlen,loglen);
	}
	else if (type == RARRAY_TAG) {
	  loglen = bytelen / 8;
	  if (show)
	    printf("%ld: RAR(%ld/%ld)  ",obj,wordlen,loglen);
	}
	else if (type == PARRAY_TAG) {
	  loglen = bytelen / 4;
	  if (show)
	    printf("%ld: PAR(%ld/%ld)  ",obj,wordlen,loglen);
	}
	else 
	  assert(0);

	for (i=0; i<((type==IARRAY_TAG)?wordlen:loglen); i++) {
	  if (show && ((i) / 8 * 8) == (i) && (i != 0))
	    printf("        ");
	  switch (type) {
	    case IARRAY_TAG: {
	      val_t field = obj[i];
	      if (show)
		printf("I(%ld)  ",field);
	      data_check(obj+i,legalHeaps,legalStarts);
	      break;
	    }
	    case PARRAY_TAG: {
	      ptr_t field = (ptr_t) obj[i];
	      if (show)
		printf("%ld   ",field);
	      ptr_check((ptr_t *)(obj+i),legalHeaps,legalStarts);
	      break;
	    }
	    case RARRAY_TAG: {
	      if (show)
		printf("%12f   ",((double *)obj)[i]);
	      break;
	    }
	  default : 
	    assert(0);
	  }
	  if (show && ((i+1) / 8 * 8) == (i+1))
	    printf("\n");
	}
	if (show)
	  printf("\n");
	if (wordlen == 0)
	  end = obj + 1;
	else 
	  end = obj + wordlen;
      }
      break;
    case SKIP_TAG: {
      int wordsSkipped = tag >> SKIPLEN_OFFSET;
      end = start + wordsSkipped;
      if (show)
	printf("%ld - %ld: SKIP %d words\n", start, end, wordsSkipped);
      assert(wordsSkipped > 0);
      break;
    }
    default:
      if (IS_FORWARDPTR(tag)) { /* tag is a forwarding pointer if it is a multiple of 4 */
	mem_t forwardstart = ((mem_t)tag) - 1;
	mem_t forwardend = NULL;
	int len;
	if (show)
	  printf("%ld: SENT(%d) -> ",obj,forwardstart + 1);
	forwardend = show_obj(forwardstart,show,legalHeaps,legalStarts);
	len = forwardend - forwardstart;
	end = start + len;
      }
      else {
	printf("\nshow_obj  tag = %d(%d) at address = %d\b",tag,GET_TYPE(tag),obj);
	assert(0);
      }
      break;
    }
  return end;
}



Bitmap_t *scan_heap(char *label, mem_t start, mem_t finish, mem_t top, 
		    Heap_t **legalHeaps, Bitmap_t **legalStarts,
		    int show, int makeBitmap)
{
  mem_t cur = start;
  Bitmap_t *curStart = makeBitmap ? CreateBitmap(finish - start) : NULL;
  if (NumGC < LEAST_GC_TO_CHECK)
    return;
  if (show) {
    printf("--------------HEAP CHECK at GC %d ------------------------------\n",NumGC);
    printf("%s %d <= %d < %d\n",label,start,finish,top);
    printf("--------------------------------------------------------------\n");
  }
  while (cur < finish) {
    if (cur < finish) {
      if (makeBitmap) {
	int pos = (cur + 1) - start;
	SetBitmapRange(curStart,pos,1);
      }
      cur = show_obj(cur,show,legalHeaps,legalStarts);
    }
  }
  if (show) {
    printf("-----------------------------------------------------------------\n\n");
  }
  return curStart;
}


void show_heap_raw(char *label, int numwords,
		   mem_t from_low, mem_t from_high,
		   mem_t to_low,   mem_t to_high)
{
  int f;
  printf("---------HEAP SHOW RAW : %s   ----------\n",label);	
  for (f=0; f<numwords; f++)
    printf("%ld: %ld       %ld: %ld\n",
	   from_low+f,*(from_low+f),
	   to_low+f,*(to_low+f));
  printf("-----------------------------------------------\n");	
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


