#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include "general.h"
#include "tag.h"
#include "memobj.h"
#include "show.h"
#include "global.h"


#define POINTER_FIELD 0
#define INT_FIELD 1
#define DOUBLE_FIELD 2

int traceError = 0;

int inHeap(ptr_t v, Heap_t *heap)
{
  return ((v >= heap->bottom) && (v <= heap->top));
}

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
int show_field(int show, int fieldType, ptr_t primary, ptr_t replica, int i, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int isNonheapPointer;
  ptr_t primaryField, replicaField;
  double primaryDoubleField, replicaDoubleField;

  if (fieldType == DOUBLE_FIELD) {
    primaryDoubleField = *((double *)(primary+i));
    if (replica)
      replicaDoubleField = *((double *)(replica+i));
  }
  else {
    primaryField = (ptr_t) primary[i];
    if (replica)
      replicaField = (ptr_t) replica[i];
  }

  isNonheapPointer = ((fieldType == POINTER_FIELD) &&
		      (IsTagData(primaryField) ||
		       IsGlobalData(primaryField) ||
		       primaryField == (ptr_t) 258));


  switch (fieldType) {
    case POINTER_FIELD: 
      if (show) 
	printf("P(%5d)  ", primaryField);     
      if (isNonheapPointer ||
	  (inHeaps(primaryField,legalHeaps,legalStarts)))
	;
      else {
	traceError = 1;
	printf("\n  !!!!TRACE ERROR: found bad pointer %d at %d[%d] failed.  GC #%d\n",primaryField,primary,i,NumGC);
	return 0;
      }
      if (replica) {
	if (isNonheapPointer) {
	  if (primaryField != replicaField) {
	    traceError = 1;
	    printf("\n  !!!!TRACE ERROR at GC %2d: replica mismatch: %d[%d] = *%d*",NumGC,primary,i,primaryField);
	    printf("\n                                              %d[%d] = *%d*\n",replica,i,replicaField);
	    return 0;
	  }
	}
	else {
	  if (primaryField != replicaField &&               /* Primary and replica may share in a generational collector */
	      (ptr_t) primaryField[-1] != replicaField) {
	    traceError = 1;
	    printf("\n  !!!!TRACE ERROR at GC %2d: ptr replica mismatch: %d[%d] = %d -> *%d*",NumGC,primary,i,primaryField,primaryField[-1]);
	    printf("\n                                                  %d[%d] = *%d*\n",replica,i,replicaField);
	    return 0;
	  }
	}
      }
      break;
    case INT_FIELD:
      if (show) 
	printf("I(%5d)  ", primaryField); 
      if (verbose) 
	if (inHeaps(primaryField,legalHeaps,legalStarts)) 
	  printf("\n !!!!TRACE WARNING at GC %2d: found suspicious int %d at %d[%d]\n",NumGC,primaryField,primary,i);
      if (replica && primaryField != replicaField) {
	traceError = 1;
	printf("\n  !!!!TRACE ERROR at GC %2d: int replica mismatch: %d[%d] = *%d*",NumGC,primary,i,primaryField);
	printf("\n                                                  %d[%d] = *%d*\n",replica,i,replicaField);
	return 0;
      }
      break;
    case DOUBLE_FIELD: 
      if (show) 
	printf("R(%10g)  ", primaryDoubleField); 
      if (replica && !(isnan(primaryDoubleField) && isnan(replicaDoubleField)) && primaryDoubleField != replicaDoubleField) {
	printf("\n  !!!!TRACE ERROR at GC %2d: double replica mismatch: %d[%d] = *%g*",NumGC,primary,i,primaryDoubleField);
	printf("\n                                                     %d[%d] = *%g*\n",replica,i,replicaDoubleField);
	return 0;
      }
      break;
    default:
      assert(0);
  }

  
  return 1;
}


/* Show the object whose raw beginning (i.e. including tag(s)) is at s */
mem_t show_obj(mem_t start, int show, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  mem_t end = NULL;
  ptr_t obj = start + 1, replica = obj; /* replica ultimately is 0 if equal to obj */
  tag_t tag = start[0];
  int i, type;
  int forwarded = IS_FORWARDPTR(tag);

  if (show && (GET_TYPE(tag) != SKIP_TAG))
    printf("%ld:  %ld", start, obj);
  while (IS_FORWARDPTR(tag)) {
    assert(replica != (ptr_t) tag);
    replica = (ptr_t) tag;
    tag = replica[-1];
    if (show)
      printf( " -> %ld", replica);
  }
  if (obj == replica)
    replica = 0;
  if (show)
    printf (": ");
  type = GET_TYPE(tag);

  switch (type) {
    case RECORD_TAG:
      {
	int len = GET_RECLEN(tag);
	int mask = GET_RECMASK(tag);
	if (mask >> len) {
	  printf("TRACE ERROR: Bad record tag %d at %d\n", tag, start);
	  assert(0);
	}
	if (show)
	  printf("REC(%d)   %ld: ", len, obj);
	for (i=0; i<len; i++) {
	  val_t field = obj[i];
	  int isPointer = 1 & (mask >> i);
	  show_field(show, isPointer ? POINTER_FIELD : INT_FIELD, obj, replica, i, legalHeaps, legalStarts);
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
	unsigned int loglen, fieldlen;
	char *typeDesc = NULL;

	if (type == IARRAY_TAG) {
	  loglen = bytelen;
	  fieldlen = wordlen;
	  typeDesc = "IAR";
	}
	else if (type == RARRAY_TAG) {
	  loglen = bytelen / 8;
	  fieldlen = loglen;
	  typeDesc = "RAR";
	}
	else if (type == PARRAY_TAG) {
	  loglen = bytelen / 4;
	  fieldlen = loglen;
	  typeDesc = "PAR";
	}
	else 
	  assert(0);
	if (show) 
	  printf("%s(%ld/%ld)  %ld: ",typeDesc,wordlen,loglen,obj);
	
	for (i=0; i<fieldlen; i++) {
	  if (show && ((i) / 8 * 8) == (i) && (i != 0))
	    printf("        ");
	  switch (type) {
	    case IARRAY_TAG: 
	      show_field(show,INT_FIELD,obj,replica,i,legalHeaps,legalStarts);
	      break;
	    case PARRAY_TAG: 
	      show_field(show,POINTER_FIELD,obj,replica,i,legalHeaps,legalStarts);
	      break;
	    case RARRAY_TAG: 
	      show_field(show,DOUBLE_FIELD,obj,replica,2*i,legalHeaps,legalStarts);
	      break;
	    default : 
	      assert(0);
	  }
	  if (show && ((i+1) / 8 * 8) == (i+1))
	    printf("\n");
	}
	if (show)
	  printf("\n");
	end = obj + wordlen;
	break;
      }
    case SKIP_TAG: {
      int wordsSkipped = tag >> SKIPLEN_OFFSET;
      end = start + wordsSkipped;
      if (show)
	printf("%ld - %ld: SKIP %d words\n", start, end, wordsSkipped);
      assert(wordsSkipped > 0);
      break;
    }
    default:
      if (IS_FORWARDPTR(tag)) 
	assert(0);
      else {
	printf("\nshow_obj  tag = %d(%d) at address = %d\b",tag,GET_TYPE(tag),obj);
	assert(0);
      }
      break;
    }
  return end;
}



void scan_heap(char *label, mem_t start, mem_t finish, mem_t top, 
	       Heap_t **legalHeaps, Bitmap_t **legalStarts,
	       int show, Bitmap_t *startMap)
{
  mem_t cur = start, next;
  if (startMap)
    ClearBitmap(startMap);
  if (show) {
    printf("--------------HEAP CHECK at GC %d ------------------------------\n",NumGC);
    printf("%s %d <= %d < %d\n",label,start,finish,top);
    printf("--------------------------------------------------------------\n");
  }
  while (cur < finish) {
    if (cur < finish) {
      if (startMap) {
	int pos = (cur + 1) - start;
	SetBitmapRange(startMap,pos,1);
      }
      next = show_obj(cur,show,legalHeaps,legalStarts);
      assert(next > cur);
      cur = next;
    }
  }
  if (show) {
    printf("-----------------------------------------------------------------\n\n");
  }
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

void memdump(char *title, unsigned int *start, int len, unsigned int *target)
{
  int i;
  printf("Memory dump start: %s\n",title);
  for (i=0; i<len; i++) {
    unsigned int *addr = start + i;
    printf("  %u: %u",addr,*addr);
    if (addr == target)
      printf("     <---------");
    printf("\n");
  }
  printf("Memory dump end: %s\n",title);
}


