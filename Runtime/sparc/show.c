#include "s.h"
#include "r.h"
#include "sparc.h"
#include <math.h>

int numErrors = 0;
int errorsToShow = 10000;

int inHeaps(ptr_t v, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  if (legalHeaps == NULL)   /* no check is performed */
    return 1; 
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
      /*
      if (!validOffset)
	printf("!!! invalid wordOffset %d\n",wordOffset);
      */
      return validOffset;
    }
  }
  return 0;
}

/* Check that pointer is either a tag, a global, or a current heap value */
static int show_field(int show, Field_t fieldType, 
		      ptr_t primary, ptr_t replica, int i, int ri,
		      Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int isNonheapPointer;
  ptr_t primaryField, replicaField;
  double primaryDoubleField, replicaDoubleField;

  if (fieldType == DoubleField) {
    primaryDoubleField = ((double *) primary)[i];
    if (replica)
      replicaDoubleField = ((double *) replica)[ri];
  }
  else {
    primaryField = (ptr_t) primary[i];
    if (replica)
      replicaField = (ptr_t) replica[ri];
  }

  isNonheapPointer = ((fieldType == PointerField) &&
		      (IsTagData(primaryField) ||
		       IsGlobalData(primaryField) ||
		       primaryField == (ptr_t) uninit_val));

  switch (fieldType) {
    case PointerField: 
    case OldPointerField: 
      if (show) 
	printf("P(%5lx)  ", (long)primaryField);     
      if ((isNonheapPointer && (fieldType == PointerField)) ||
	  fieldType == OldPointerField ||
	  inHeaps(primaryField,legalHeaps,legalStarts))
	;
      else {
	if (inHeaps(primaryField,legalHeaps,NULL)) {
	  if (numErrors++ < errorsToShow)
	    fprintf(stderr,"\n  !!!!TRACE ERROR: bad pointer (in range) %lx at %lx[%d] failed.  GC #%d\n",(long)primaryField,(long)primary,i,NumGC);
	}
	else {
	  if (numErrors++ < errorsToShow)
	    fprintf(stderr,"\n  !!!!TRACE ERROR: bad pointer (out of range) %lx at %lx[%d] failed.  GC #%d\n",(long)primaryField,(long)primary,i,NumGC);
	}
	return 0;
      }
      if (replica) {
	if (isNonheapPointer) {
	  if (primaryField != replicaField &&
	    (numErrors++ < errorsToShow)) {
	    fprintf(stderr,"\n  !!!!TRACE ERROR at GC %2d: replica mismatch: %lx[%d] = *%lx*",NumGC,(long)primary,i,(long)primaryField);
	    fprintf(stderr,"\n                                              %lx[%d] = *%lx*\n",(long)replica,ri,(long)replicaField);
	    return 0;
	  }
	}
	else {
	  if (primaryField != replicaField &&               /* Primary and replica may share in a generational collector */
	      (ptr_t) primaryField[-1] != replicaField &&
	      (numErrors++ < errorsToShow)) {
	    fprintf(stderr,"\n  !!!!TRACE ERROR at GC %2d: ptr replica mismatch: %lx[%d] = %lx -> *%d*",NumGC,
		   (long)primary,i,(long)primaryField,primaryField[-1]);
	    fprintf(stderr,"\n                                                  %lx[%d] = *%lx*\n",
		   (long)replica,ri,(long)replicaField);
	    return 0;
	  }
	}
      }
      break;
    case IntField:
      if (show) 
	printf("I(%5lx)  ", (long)primaryField); 
      if (verbose) 
	if (inHeaps(primaryField,legalHeaps,legalStarts)) 
	  printf("\n !!!!TRACE WARNING at GC %2d: found suspicious int %ld at %lx[%d]\n",NumGC,(long)primaryField,(long)primary,i);
      if (replica && primaryField != replicaField &&
	  (numErrors++ < errorsToShow)) {
	fprintf(stderr,"\n  !!!!TRACE ERROR at GC %2d: int replica mismatch: %lx[%d] = *%lx*",NumGC,(long)primary,i,(long)primaryField);
	fprintf(stderr,"\n                                                  %lx[%d] = *%lx*\n",(long)replica,ri,(long)replicaField);
	return 0;
      }
      break;
    case DoubleField: 
      if (show) 
	printf("R(%10g)  ", primaryDoubleField); 
      if (replica && !(isnan(primaryDoubleField) && isnan(replicaDoubleField)) && primaryDoubleField != replicaDoubleField) {
	fprintf(stderr,"\n  !!!!TRACE ERROR at GC %2d: double replica mismatch: %lx[%d] = *%g*",NumGC,(long)primary,i,primaryDoubleField);
	fprintf(stderr,"\n                                                     %lx[%d] = *%g*\n",(long)replica,ri,replicaDoubleField);
	return 0;
      }
      break;
    default:
      DIE("bad field type in show_field");
  }

  
  return 1;
}


/* Show the object whose raw beginning (i.e. including tag(s)) is at s */
mem_t show_obj(mem_t start, ptr_t *objRef, int show, ShowType_t replicaType, Heap_t **legalHeaps, Bitmap_t **legalStarts)
{
  int i, type;
  mem_t temp, end = NULL;
  tag_t tag;
  ptr_t obj, replica;                 

  /* Skip past all extra tags (belonging to the obj) at the beginning */
  for (temp = start; (*temp == SEGSTALL_TAG) || (*temp == SEGPROCEED_TAG); temp++)
    ;
  tag = *temp;
  obj = temp + 1;
  replica = obj;                       /* replica ultimately is 0 if equal to obj */

  if (show && !IS_SKIP_TAG(tag)) {
    printf("%lx:  ", (long)start);
    if (start < obj - 1) {
      printf("[");
      for (temp = start; temp < start - 1; temp++)
	printf("%d%s", *temp, (temp < start - 2) ? ", " : "");
      printf("]");
    }
  }
  while (TAG_IS_FORWARD(tag)) {
    assert(replica != (ptr_t) tag);
    replica = (ptr_t) tag;
    tag = replica[-1];
    if (show) {
      printf( " -> %lx", (long)replica);
      /* printf("  TRACE ERROR: forwarding pointer is in same heap(s).  "); */
    }
  }
  if (obj == replica || replicaType == NoReplica)
    replica = 0;
  if (show)
    printf (": ");
  type = GET_TYPE(tag);

  switch (type) {
    case RECORD_TYPE: {
      int len = GET_RECLEN(tag);
      int mask = GET_RECMASK(tag);
      if (mask >> len) {
	fprintf(stderr,"TRACE ERROR: Bad record tag %d at %lx\n", tag, (long)start);
	DIE("bad record tag in show_obj");
      }
      if (show)
	printf("REC(%d)   %lx: ", len, (long)obj);
      for (i=0; i<len; i++) {
	int isPointer = 1 & (mask >> i);
	show_field(show, isPointer ? PointerField : IntField, obj, replica, i, i, legalHeaps, legalStarts);
      }
      if (show)
	printf("\n");
      end = obj + len;
    }
    break;
    case MIRROR_PTR_ARRAY_TYPE: {
      unsigned int bytelen = GET_ANY_ARRAY_LEN(tag);
      unsigned int wordlen = (bytelen + 3) / 4;
      unsigned int loglen = wordlen / 2;
      int i;
      if (show) 
	printf("MPA(%ld/%ld)  %lx: ",(long)wordlen,(long)loglen,(long)obj);
      for (i=0; i<loglen; i++) {
	if (show && ((i) / 4 * 4) == (i) && (i != 0))
	  printf("        ");
	if (!mirrorArray) {
	  assert(primaryArrayOffset == 0);
	  show_field(show, PointerField, obj, replica, 2*i, 2*i, legalHeaps, legalStarts);
	}
	else {
	  int doReplica = replicaType != NoReplica;
	  if (replicaType == SelfReplica && replica == NULL)  /* self is replica if in tenured space */
	    replica = obj;
	  if (primaryArrayOffset == 0) {
	    show_field(show, doReplica ? OldPointerField : PointerField, obj, replica, 2*i,   2*i+1, legalHeaps, legalStarts);
	    show_field(show, doReplica ? PointerField     : IntField,     obj, NULL,    2*i+1, 2*i,   legalHeaps, legalStarts);
	  }
	  else {
	    show_field(show, doReplica ? PointerField     : IntField,     obj, NULL,    2*i,   2*i+1, legalHeaps, legalStarts);
	    show_field(show, doReplica ? OldPointerField : PointerField, obj, replica, 2*i+1, 2*i,   legalHeaps, legalStarts);
	  }
	  if (show)
	    printf("   ");
	}
      }
      if (show)
	printf("\n");
	end = obj + wordlen;
      break;
    }
    case WORD_ARRAY_TYPE:
    case QUAD_ARRAY_TYPE:
    case PTR_ARRAY_TYPE:
      {
	unsigned int bytelen = GET_ANY_ARRAY_LEN(tag);
	unsigned int wordlen = (bytelen + 3) / 4;
	unsigned int loglen, fieldlen;
	char *typeDesc = NULL;

	switch (type) {
	  case WORD_ARRAY_TYPE:
	    loglen = bytelen;
	    fieldlen = wordlen;
	    typeDesc = "IAR";
	    break;
	  case QUAD_ARRAY_TYPE:
	    loglen = bytelen / 8;
	    fieldlen = loglen;
	    typeDesc = "RAR";
	    break;
          case PTR_ARRAY_TYPE:
	    loglen = bytelen / 4;
	    fieldlen = loglen;
	    typeDesc = "PAR";
	    break;
	  default: DIE("impossible");
	}
	if (show) 
	  printf("%s(%ld/%ld)  %lx: ",typeDesc,(long)wordlen,(long)loglen,(long)obj);
	
	for (i=0; i<fieldlen; i++) {
	  if (show && ((i) / 8 * 8) == (i) && (i != 0))
	    printf("        ");
	  switch (type) {
	    case WORD_ARRAY_TYPE: show_field(show,IntField,obj,replica,i,i,legalHeaps,legalStarts);  break;
	    case PTR_ARRAY_TYPE: show_field(show,PointerField,obj,replica,i,i,legalHeaps,legalStarts); break;
	    case QUAD_ARRAY_TYPE: show_field(show,DoubleField,obj,replica,i,i,legalHeaps,legalStarts); break;
	      break;
	    default : 
	      DIE("impossible");
	  }
	  if (show && ((i+1) / 8 * 8) == (i+1))
	    printf("\n");
	}
	if (show)
	  printf("\n");
	end = obj + wordlen;
	break;
      }
    case OTHER_TYPE: 
      if (IS_SKIP_TAG(tag)) {
	int wordsSkipped = GET_SKIPWORD(tag);
	end = start + wordsSkipped;
	if (show)
	  printf("%lx - %lx: SKIP %d words\n", (long)start, (long)end, wordsSkipped);
	assert(wordsSkipped > 0);
      }
      else if (tag == MIRROR_GLOBAL_PTR_TAG) {
	if (show)
	  printf("MIRROR_GLOBAL   %lx: ", (long)obj);
	if (!mirrorGlobal) {
	  assert(primaryGlobalOffset == 0);
	  show_field(show, PointerField, obj, replica, 0, 0, legalHeaps, legalStarts);
	}
	else {
	  int doReplica = replicaType != NoReplica;
	  if (replicaType == SelfReplica && replica == NULL)  /* self is replica if in tenured space */
	    replica = obj;
	  if (primaryGlobalOffset == 0) {
	    show_field(show, doReplica ? OldPointerField : PointerField, obj, replica, 0, 1, legalHeaps, legalStarts);
	    show_field(show, doReplica ? PointerField : IntField, obj, NULL, 1, 0, legalHeaps, legalStarts);
	  }
	  else {
	    show_field(show, doReplica ? PointerField : IntField, obj, NULL, 0, 1,  legalHeaps, legalStarts);
	    show_field(show, doReplica ? OldPointerField : PointerField, obj, replica, 1, 0, legalHeaps, legalStarts);
	  }
	}
	if (show)
	  printf("\n");
	end = obj + 2;
      }
      else {
	fprintf(stderr,"\nshow_obj  tag = %d(%d) at address = %lx\n",tag,GET_TYPE(tag),(long)obj);
	DIE("show_obj");
      }
      break;
  }
  *objRef = obj;
  return end;
}



void scan_heap(char *label, mem_t start, mem_t finish, mem_t top, 
	       Heap_t **legalHeaps, Bitmap_t **legalStarts,
	       int show, ShowType_t replicaType, Bitmap_t *startMap)
{
  mem_t cur = start, next;
  if (startMap)
    ClearBitmap(startMap);
  if (show) {
    printf("--------------HEAP CHECK at GC %d ------------------------------\n",NumGC);
    printf("%s %lx <= %lx < %lx\n",label,(long)start,(long)finish,(long)top);
    printf("--------------------------------------------------------------\n");
  }
  while (cur < finish) {
    if (cur < finish) {
      ptr_t obj = NULL;
      next = show_obj(cur,&obj,show,replicaType,legalHeaps,legalStarts);
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
    printf("%lx: %lx       %lx: %lx\n",
	   (long)(from_low+f),(long)*(from_low+f),
	   (long)(to_low+f),(long)*(to_low+f));
  printf("-----------------------------------------------\n");	
}

void memdump(char *title, unsigned int *start, int len, unsigned int *target)
{
  int i;
  printf("Memory dump start: %s\n",title);
  for (i=0; i<len; i++) {
    unsigned int *addr = start + i;
    printf("  %lx: %lx",(long)addr,(long)*addr);
    if (addr == target)
      printf("     <---------");
    printf("\n");
  }
  printf("Memory dump end: %s\n",title);
}


