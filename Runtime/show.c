#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include "general.h"
#include "tag.h"
#include "memobj.h"
#include "show.h"
#include "global.h"
#include "gc.h"

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
      int validOffset;
      if (curStart == NULL)
	return 1;
      validOffset = IsSet(curStart, wordOffset);
      if (!validOffset)
	printf("!!! invalid wordOffset %d\n",wordOffset);
      return validOffset;
    }
  }
  return 0;
}

/* Check that pointer is either a tag, a global, or a current heap value */
static int show_field(int show, Field_t fieldType, ptr_t primary, ptr_t replica, int i, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int isNonheapPointer;
  ptr_t primaryField, replicaField;
  double primaryDoubleField, replicaDoubleField;

  if (fieldType == DoubleField) {
    primaryDoubleField = ((double *) primary)[i];
    if (replica)
      replicaDoubleField = ((double *) replica)[i];
  }
  else {
    primaryField = (ptr_t) primary[i];
    if (replica)
      replicaField = (ptr_t) replica[i];
  }

  isNonheapPointer = ((fieldType == PointerField) &&
		      (IsTagData(primaryField) ||
		       IsGlobalData(primaryField) ||
		       primaryField == (ptr_t) 258));

  switch (fieldType) {
    case PointerField: 
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
    case IntField:
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
    case DoubleField: 
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
mem_t show_obj(mem_t start, ptr_t *objRef, int show, int doReplica, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int i, type;
  mem_t temp, end = NULL;
  tag_t tag;
  ptr_t obj, replica;                 

  /* Skip past all the extra OTHER_TYPE tags at the beginning */
  for (temp = start; GET_TYPE(*temp) == OTHER_TYPE; temp++)
    ;
  tag = *temp;
  obj = temp + 1;
  replica = obj;                       /* replica ultimately is 0 if equal to obj */

  if (show && (GET_TYPE(tag) != SKIP_TYPE)) {
    printf("%ld:  ");
    if (start < obj - 1) {
      printf("[");
      for (temp = start; temp < start - 1; temp++)
	printf("%d%s", *temp, (temp < start - 2) ? ", " : "");
      printf("]");
    }
    printf("%ld", start, obj);
  }
  while (TAG_IS_FORWARD(tag)) {
    assert(replica != (ptr_t) tag);
    replica = (ptr_t) tag;
    tag = replica[-1];
    if (show) {
      printf( " -> %ld", replica);
      /* printf("  TRACE ERROR: forwarding pointer is in same heap(s).  "); */
    }
  }
  if (obj == replica || !doReplica)
    replica = 0;
  if (show)
    printf (": ");
  type = GET_TYPE(tag);

  switch (type) {
    case RECORD_TYPE: {
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
	show_field(show, isPointer ? PointerField : IntField, obj, replica, i, legalHeaps, legalStarts);
      }
      if (show)
	printf("\n");
      end = obj + len;
    }
    break;
    case IARRAY_TYPE:
    case PARRAY_TYPE:
    case RARRAY_TYPE:
      {
	unsigned int bytelen = GET_ARRLEN(tag);
	unsigned int wordlen = (bytelen + 3) / 4;
	unsigned int loglen, fieldlen;
	char *typeDesc = NULL;

	if (type == IARRAY_TYPE) {
	  loglen = bytelen;
	  fieldlen = wordlen;
	  typeDesc = "IAR";
	}
	else if (type == RARRAY_TYPE) {
	  loglen = bytelen / 8;
	  fieldlen = loglen;
	  typeDesc = "RAR";
	}
	else if (type == PARRAY_TYPE) {
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
	    case IARRAY_TYPE: 
	      show_field(show,IntField,obj,replica,i,legalHeaps,legalStarts);
	      break;
	    case PARRAY_TYPE: 
	      show_field(show,PointerField,obj,replica,i,legalHeaps,legalStarts);
	      break;
	    case RARRAY_TYPE: 
	      show_field(show,DoubleField,obj,replica,i,legalHeaps,legalStarts);
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
    case SKIP_TYPE: {
      int wordsSkipped = tag >> SKIPLEN_OFFSET;
      end = start + wordsSkipped;
      if (show)
	printf("%ld - %ld: SKIP %d words\n", start, end, wordsSkipped);
      assert(wordsSkipped > 0);
      break;
    }
    default:
      printf("\nshow_obj  tag = %d(%d) at address = %d\b",tag,GET_TYPE(tag),obj);
      assert(0);
  }
  *objRef = obj;
  return end;
}



void scan_heap(char *label, mem_t start, mem_t finish, mem_t top, 
	       Heap_t **legalHeaps, Bitmap_t **legalStarts,
	       int show, int doReplica, Bitmap_t *startMap)
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
      ptr_t obj = NULL;
      next = show_obj(cur,&obj,show,doReplica,legalHeaps,legalStarts);
      assert(obj != NULL);
      assert(next > cur);
      if (startMap) {
	int pos = obj - start;
	SetBitmapRange(startMap,pos,1);
      }
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


