/* Not thread-safe */
#include "general.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <string.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "memobj.h"
#include "thread.h"
#include "show.h"
#ifdef alpha_osf
#include <c_asm.h>
#endif

#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "general.h"
#include "forward.h"



const int stall = -2;

void SetRange(range_t *range, mem_t low, mem_t high)
{
  range->low = low;
  range->high = high;
  range->diff = (high - low) * (sizeof (val_t));
}

/* ------------------ Forwarding Routines ------------------ */

int getPointerLocations(ptr_t obj, Queue_t *locs)
{
  tag_t tag = obj[-1];

  switch (GET_TYPE(tag)) {
    case RECORD_TAG: {
      int i, count = 0;
      int len = GET_RECLEN(tag);
      int curMask = GET_RECMASK(tag);
      for (i=0; i<len; i++) {
	if (curMask & 1) {   /* low bit set means current field is pointer */
	  Enqueue(locs, (loc_t) (obj + i));
	  count++;
	}
	curMask >>= 1;
      }
      assert(curMask == 0);
      return count;
    }
    case PARRAY_TAG: {
      int byteLen = GET_ARRLEN(tag);
      int wordLen = byteLen >> 2;
      int i;
      for (i=0; i<wordLen; i++)
	Enqueue(locs, (loc_t) (obj + i));
      return wordLen;
    }
    case IARRAY_TAG: return 0;
    case RARRAY_TAG: return 0;
    case SKIP_TAG: { printf("SKIP_TAG found in getPointerLocation\n"); assert(0); }
    case FORWARD_TAG: { printf("FORWARD_TAG found in getPointerLocation\n"); assert(0); }
    default: { printf("Unknown tag %d found in getPointerLocation\n",tag); assert(0); }
  }
  assert(0);
}


/* Returns object length in bytes */
unsigned long objectLength(ptr_t obj)
{
  tag_t tag = (tag_t) obj[-1];

  if (tag == SKIP_TAG)
    return 4;
  switch (GET_TYPE(tag))
  {
    case IARRAY_TAG:
    case RARRAY_TAG:
    case PARRAY_TAG:
    {
      int bytelen = GET_ARRLEN(tag);
      if (bytelen == 0)
	return 8;
      else
	return 4 + ((bytelen + 3) / 4) * 4;
    }
  case FORWARD_TAG:
    {
      ptr_t newobj = (ptr_t) obj[0];
      return objectLength(newobj);
    }
  case RECORD_TAG:
    {
      int numFields = GET_RECLEN(tag);
      if (numFields == 0)  /* There should be no empty records */
	assert(0);
      return 4 * (1 + numFields);
    }
  case SKIP_TAG:
  default:
    {
      mem_t tagstart = (mem_t) (obj - 1);
      printf("bad tag %d at %d\n",tag,tagstart);
      memdump("",tagstart-10,30,tagstart);
      printf("\n\n\n");
      printf("NumGC is %d\n",NumGC);
      assert(0);
    }
  } /* case */
}

/* This should not be called directly. */
mem_t forward(ploc_t vpp, mem_t alloc_ptr)
{
  ptr_t v = *vpp;
  int tag;
  unsigned int a,b;

  /* read some garbage to simulate write-allocate */
  int foobar = *(alloc_ptr + 4); 

  tag = v[-1];

  switch (tag)
    {
    case TAG_REC_INT:
    case TAG_REC_TRACE:
      {
	alloc_ptr[0] = v[-1];
	alloc_ptr[1] = v[0];
	v[-1] = FORWARD_TAG;
	v[0] = (val_t)(alloc_ptr+1);
	*vpp = (alloc_ptr+1);
	alloc_ptr += 2;
	return alloc_ptr;
      }
    case TAG_REC_INTINT:
    case TAG_REC_TRACEINT:
    case TAG_REC_INTTRACE:
    case TAG_REC_TRACETRACE:
      {
	alloc_ptr[0] = v[-1];
	alloc_ptr[1] = v[0];
	alloc_ptr[2] = v[1];
	v[-1] = FORWARD_TAG;
	v[0] = (val_t)(alloc_ptr+1);
	*vpp = (alloc_ptr+1);
	alloc_ptr += 3;
	return alloc_ptr;
      }
    default:
      break;
    }

  switch (GET_TYPE(tag))
    {
    case IARRAY_TAG:
    case PARRAY_TAG:
    case RARRAY_TAG:
      {
	int byteLen = GET_ARRLEN(tag);
	int wordLen = (byteLen + 3) / 4;
	if (wordLen == 0)   	/* empty arrays have 1 word of storage */
	  wordLen = 1;

	/* if real array, odd-word-align the pointer so data is even-word-aligned */
	if (GET_TYPE(tag) == RARRAY_TAG) {
	  int temp = (int) (alloc_ptr);
	  if ((temp & 7) == 0) 
	    *(alloc_ptr++) = SKIP_TAG;
	}
	
	bcopy((char *)(v-1), (char *)(alloc_ptr), 4*(1+wordLen));
	v[-1] = FORWARD_TAG;
	v[0] = (val_t) (alloc_ptr + 1);
	*vpp = (ptr_t) (alloc_ptr + 1);
	alloc_ptr += (1 + wordLen);
	return alloc_ptr;
      }
    case RECORD_TAG:
      {
	mem_t rawstart = v-1;
	tag_t tag = v[-1];
	int curlen = GET_RECLEN(tag);
	if (curlen > RECLEN_MAX)
	  curlen = RECLEN_MAX;
	if (paranoid && (GET_RECMASK(tag) >> curlen) != 0) {
	  printf("BAD RECORD TAG\n");
	  assert(0);
	}

	switch (curlen)
	  {
	  case 0: 
	    assert(0);  /* empty records are not permitted */
	  case 1:
	    {
	      alloc_ptr[0] = rawstart[0];
	      alloc_ptr[1] = rawstart[1];
	      v[-1] = FORWARD_TAG;
	      v[0] = (val_t) (alloc_ptr + 1);
	      *vpp = (ptr_t) (v[0]);
	      alloc_ptr += 2;
	      return alloc_ptr;
	    }
	  case 2:
	    {
	      alloc_ptr[0] = rawstart[0];
	      alloc_ptr[1] = rawstart[1];
	      alloc_ptr[2] = rawstart[2];
	      v[-1] = FORWARD_TAG;
	      v[0] = (val_t) (alloc_ptr + 1);
	      *vpp = (ptr_t) (v[0]);
	      alloc_ptr += 3;
	      return alloc_ptr;
	    }
	  case 3:
	    {
	      alloc_ptr[0] = rawstart[0];
	      alloc_ptr[1] = rawstart[1];
	      alloc_ptr[2] = rawstart[2];
	      alloc_ptr[3] = rawstart[3];
	      v[-1] = FORWARD_TAG;
	      v[0] = (val_t) (alloc_ptr + 1);
	      *vpp = (ptr_t) v[0];
	      alloc_ptr += 4;
	      return alloc_ptr;
	    }
	  default:
	    bcopy((char *)rawstart,(char *)(alloc_ptr),4*(1+curlen));
	    v[-1] = FORWARD_TAG;
	    v[0] = (val_t) (alloc_ptr + 1);
	    *vpp = (ptr_t) v[0];
	    alloc_ptr += (1 + curlen);
	    return alloc_ptr;
	  }
	break;
      }
    case FORWARD_TAG:
      *vpp = (ptr_t) v[0];
      return alloc_ptr;
    case SKIP_TAG:
      printf("\n\nv = %d\n",v);
      printf("tag = %d\n",tag);
      BUG("forward: IMPOSSIBLE to get SKIP_TAG");
    default:
      printf("\n\nv = %d\n",v);
      printf("tag = %d\n",tag);
      BUG("forward: IMPOSSIBLE TAG");
    }
  foobar++;
  assert(0);
}


bool_t forward_atomic(ploc_t vpp, mem_t *alloc_ptr, mem_t *limit_ptr, Heap_t *toheap)
{
  ptr_t white = *vpp;              /* old object must be in from space */
  ptr_t obj;                       /* forwarded object */
  tag_t tag;                       /* original tag */
  mem_t alloc = *alloc_ptr;
  mem_t limit = *limit_ptr;

  assert(white < toheap->bottom ||
	 white >= toheap->top);     /* white object cannot be in to space */

  /* Check for forward tag first since ldl_l/stl_c is expensive */
  if (white[-1] == FORWARD_TAG) {
     *vpp = (ptr_t) white[0];
     return 0;
   }

 /* Atomically try commiting to be the copier; 
    we exit this when there is some copier */

#ifdef alpha_osf
  {
    int done = 0;
    while (!done) {
      /*    asm("ldl_l %0,-4(%1)" : "=i" (tag) : "i" (white)); */
      tag = asm("ldl_l %v0,-4(%a0)",white); 
      if (tag == stall) 
	done = 1;
      else 
	done |= asm("stl_c %a0,-4(%a1) ; mov %a0,%v0",stall,white);
    }
    assert(tag != stall);
  }
#endif

#ifdef sparc
    {
       mem_t tagloc = white - 1;
       tag = *tagloc;
       if (tag == FORWARD_TAG)  /* Somebody already completed forwarding */
	 ;
       else if (tag == stall) { /* Somebody grabbed it but did not finish forwarding */
	 while (tag == stall)
	   tag = white[-1];
	 assert(tag == FORWARD_TAG);
       }
       else {                   /* Try to be the copier */
	 /* Example of a SPARC ld statement with gcc asm
	    int *ptr;
	    int val;
	    asm("ld   [%1],%0" : "=r" (val) : "r" (ptr)); 

	    The following tries to atomicaly swap in the stall tag by comparing with original tag.
	    Note that registers that are input and output are specified twice with the input
	    use referring to the output register.
	 */
	 val_t localStall = stall;
	 asm("cas [%2],%3,%0" : "=r" (localStall) : "0" (localStall), "r" (tagloc), "r" (tag)); 
	 /* localStall == tag     : we are the copier
	    localStall == stall   : somebody else is the copier and was in the middle of its operation
	    localStall == FORWARD : somebody else is the copier and forwarded it already */
	 if (localStall != tag) { /* we are not copier, wait til tag is FORWARD */
	   tag = white[-1];
	   while (tag == stall)
	     tag = white[-1];
	   assert(tag == FORWARD_TAG);
	 }
       }
    }
#endif


  /* In the ensuing code, the tag must be restored only after the forwarding address is written */
  switch (tag)
    {
    case TAG_REC_INT:
    case TAG_REC_TRACE:
      {
	if (alloc+2 >= limit) {
	  GetHeapArea(toheap,pagesize,alloc_ptr,limit_ptr);
	  alloc = *alloc_ptr;
	  limit = *limit_ptr;
	  assert(alloc + 2 < limit);
	}
	obj = alloc + 1;
	alloc += 2;
	*alloc_ptr = alloc;
	obj[-1] = tag;
	obj[0] = white[0];
	*vpp = obj;
	white[0] = (val_t) obj;
	flushStore();
	white[-1] = FORWARD_TAG; /* Write tag last */
	assert(obj[-1] != FORWARD_TAG);
	return 1;
      }
    case TAG_REC_INTINT:
    case TAG_REC_TRACEINT:
    case TAG_REC_INTTRACE:
    case TAG_REC_TRACETRACE:
      {
	if (alloc+3 >= limit) {
	  GetHeapArea(toheap,pagesize,alloc_ptr,limit_ptr);
	  alloc = *alloc_ptr;
	  limit = *limit_ptr;
	  assert(alloc + 3 < limit);
	}
	obj = alloc + 1;
	alloc += 3;
	*alloc_ptr = alloc;
	obj[-1] = tag;
	obj[0] = white[0];
	obj[1] = white[1];
	*vpp = obj;
	white[0] = (val_t) obj;
	flushStore();
	white[-1] = FORWARD_TAG; /* Write tag last */
	assert(obj[-1] != FORWARD_TAG);
	return 1;
      }
    default:
      break;
    }

  switch (GET_TYPE(tag))
    {
    case IARRAY_TAG:
    case PARRAY_TAG:
    case RARRAY_TAG:
      {
	mem_t rawstart = white-1;
	int bytelen = GET_ARRLEN(tag);
	int wordlen = (bytelen + 3) / 4;
	int i;

	/* empty arrays still have 1 word of storage for the forwarding pointer */
	if (wordlen == 0)
	  wordlen = 1;

	/* one for the tag and one for alignment */
	if (alloc + (2+wordlen) >= limit) {
	  int requestBytes = 4 * (2 + wordlen);
	  int request = (requestBytes + pagesize - 1) / pagesize * pagesize;
	  GetHeapArea(toheap,request,alloc_ptr,limit_ptr);
	  alloc = *alloc_ptr;
	  limit = *limit_ptr;
	  assert (alloc + (2+wordlen) <= limit);
	}

	if (GET_TYPE(tag) == RARRAY_TAG) {
	  int alloc_int = (int) (alloc);
	  /* odd-word align the pointer so data is even-aligned */
	  if ((alloc_int & 7) == 0) {
	    *alloc = SKIP_TAG;
	    alloc++;
	  }
	}
	obj = alloc + 1;
	alloc += (1 + wordlen);
	*alloc_ptr = alloc;
	obj[-1] = tag;	
	bcopy((const char *)white, (char *)obj, 4*wordlen);
	*vpp = obj;
	white[0] = (val_t) obj;
	flushStore();
	white[-1] = FORWARD_TAG;
	assert(obj[-1] != FORWARD_TAG);
	return 1;
      }
    case RECORD_TAG:
      {
	/* We must use tag to initialize curtag and NOT use white[-1] */
	int curtag = tag;
	int numfields = GET_RECLEN(curtag);

	/* Empty records not allowed.  Records with less than 1 or 2 components alrady handled */
	if (numfields <= 2) {
	  printf("bad record tag %d at %d\n",tag,white - 1);
	  assert(0);
	}

	if (alloc+1+numfields >= limit) {
	  GetHeapArea(toheap,pagesize,alloc_ptr,limit_ptr);
	  alloc = *alloc_ptr;
	  limit = *limit_ptr;
	  assert (alloc+1+numfields < limit);
	}
	obj = alloc + 1;
	alloc += 1+numfields;
	*alloc_ptr = alloc;
	bcopy((char *)(white),(char *)(obj),4*(numfields));
	obj[-1] = tag;
	*vpp = obj;
	white[0] = (val_t) obj;
	flushStore();
	white[-1] = FORWARD_TAG;
	assert(obj[-1] != FORWARD_TAG);
	return 1;
      }
    case FORWARD_TAG:
      {
	ptr_t gray = (ptr_t) white[0];
	printf("forward_atomic FORWARD_TAG gray = %d\n",gray);
	assert(tag == FORWARD_TAG);
	assert(gray[-1] != FORWARD_TAG);
	*vpp = gray;
	return 0;
      }
    case SKIP_TAG:
    default:
      printf("\n\nforward_atomic: BAD TAG: white = %d, tag = %d\n",white,tag);
      BUG("forward_atomic: IMPOSSIBLE TAG");
    }
  assert(0);
}


/* ------------ These forwarding routines/macros are the ones to use --------- */

void check_ptr(ploc_t ptrLoc)
{
  if (*ptrLoc >= (mem_t) 42000 && *ptrLoc <= (mem_t) 70000) {
    printf("A paranoid value of %d at %d failed check_ptr\n", *ptrLoc, ptrLoc);
    assert(0);
  }
}

mem_t forward1_root_lists(Queue_t *root_lists, mem_t to_ptr, 
			       range_t *from_range, range_t *to_range)
{
  long i, j, rlen = QueueLength(root_lists);
  for (i=0; i<rlen; i++) {
      Queue_t *roots = QueueAccess(root_lists,i);
      int qlen = QueueLength(roots);
      for (j=0; j<qlen; j++) {
	  ploc_t temp = (ploc_t) QueueAccess(roots,j);
	  if (paranoid)
	    check_ptr(temp);
	  to_ptr = forward1(temp,to_ptr,from_range);
	  NumRoots++;
	}
    }
  return to_ptr;
}


mem_t forward2_root_lists(Queue_t *root_lists, mem_t to_ptr, 
			  range_t *from, range_t *from2, range_t *to,
			  range_t *large, Queue_t *largeRoots)
{
  long i, j, rlen = QueueLength(root_lists);
  for (i=0; i<rlen; i++) {
      Queue_t *roots = QueueAccess(root_lists,i);
      int qlen = QueueLength(roots);
      for (j=0; j<qlen; j++) {
	  ploc_t temp = (ploc_t) QueueAccess(roots,j);
	  if (paranoid)
	    check_ptr(temp);
	  to_ptr = forward2(temp,to_ptr,from,from2,large,largeRoots);
	  NumRoots++;
	}
    }
  return to_ptr;
}

mem_t forward1_writelist(SysThread_t *sysThread, mem_t alloc,
			 range_t *from, range_t *to)
{
  ploc_t curLoc = sysThread->writelistStart;
  ploc_t end = sysThread->writelistCursor;
  while (curLoc < end) {
    ploc_t field = (ploc_t)(*(curLoc++));
    ptr_t data = *field;
    if (NotInRange(data,to)) 
      alloc = forward1(field,alloc,from);
  }
  return alloc;
}

void forward1_writelist_atomic_stack(mem_t *alloc, mem_t *limit,
				     Heap_t *toheap, range_t *from, range_t *to, SysThread_t *sysThread)
{
  ploc_t curLoc = sysThread->writelistStart;
  ploc_t end = sysThread->writelistCursor;
  while (curLoc < end) {
    ploc_t field = (ploc_t)(*(curLoc++));
    ptr_t data = *field;
    if (NotInRange(data,to))
      forward1_atomic_stack(field,alloc,limit,toheap,from,sysThread);
  }
}



/* -------------------------------------------------------------- */

void update_object_profile(Object_Profile_t *prof, ptr_t objstart)
{
  mem_t tagstart = objstart - 1;
  int tag = *tagstart;
  switch(GET_TYPE(tag))
    {
    case IARRAY_TAG:
      prof->IArray++;
      prof->IArrayWord += ((GET_ARRLEN(tag) + 3) / 4) + 1;
      break;
    case RARRAY_TAG:
      prof->RArray++;
      prof->RArrayWord += GET_ARRLEN(tag) / 4 + 1;
      break;
    case PARRAY_TAG:
      prof->PArray++;
      prof->PArrayWord += GET_ARRLEN(tag) / 4 + 1;
      break;
    case RECORD_TAG:
      {
	int len = GET_RECLEN(tag);
	prof->Record++;
	if (len == 2)
	  prof->Pair++;
	prof->RecordWord += len + 1;
      }
      return;
    case SKIP_TAG:
    case FORWARD_TAG:
      return;
    default:
      printf("objstart = %d\n",objstart);
      printf("Got a tag of %d\n",tag);
      assert(0);
      return;
    }
}



/* --------------- Scanning routines ----------------------- */

mem_t scan1_object(mem_t *where, mem_t alloc,
		   range_t *from, range_t *to)
{
  ptr_t cur = (ptr_t) (*where);
  tag_t tag = cur[0];
  unsigned int type = GET_TYPE(tag);

  switch (type)
    {
    case RECORD_TAG:
      {
	int i, fieldlen = GET_RECLEN(tag);
	mem_t end = cur + 1 + fieldlen;
	unsigned mask = GET_RECMASK(tag);
	cur++;
	for (; cur<end; cur++, mask >>= 1)
	  if (mask & 1) {
	    if (paranoid)
	      check_ptr((ploc_t)cur);
	    alloc = forward1((ploc_t)cur,alloc,from);
	  }
	break;
      }
    case IARRAY_TAG:
    case RARRAY_TAG: 
      {
	unsigned int wordLen = (GET_ARRLEN(tag) + 3) / 4; /* IARRAY len might not be mult of 4 */
	if (wordLen == 0)
	  wordLen = 1;
	cur += 1 + wordLen;
	break;
      }
    case PARRAY_TAG:
      {
	unsigned int wordLen = GET_ARRLEN(tag) / 4;
	if (wordLen == 0)
	  cur += 2;
	else {
	    unsigned int *end = cur + 1 + wordLen;
	    cur++;
	    while (cur < end) {
		if (paranoid)
		  check_ptr((ploc_t)cur);
		alloc = forward1((ploc_t)cur,alloc,from);
		cur++;
	      }
	  }
	break;
      }
    case SKIP_TAG:
      cur++;
      break;
    case FORWARD_TAG:
    default:
      printf("\n\nscan1_object found bad tag = %d at cur=%d\n",tag,cur);
      assert(0);
    }
  (*where) = cur;
  return alloc;
}


mem_t scan2_object(mem_t *where, mem_t alloc,
		   range_t *from, range_t *from2,  range_t *to,
		   range_t *large, Queue_t *largeRoots)
{
  ptr_t cur = (ptr_t) (*where);
  tag_t tag = cur[0];
  unsigned int type = GET_TYPE(tag);

  switch (type)
    {
    case RECORD_TAG:
      {
	int i, fieldlen = GET_RECLEN(tag);
	mem_t end = cur + 1 + fieldlen;
	unsigned mask = GET_RECMASK(tag);
	cur++;
	for (; cur<end; cur++, mask >>= 1)
	  if (mask & 1) {
	    if (paranoid)
	      check_ptr((ploc_t)cur);
	    alloc = forward2((ploc_t)cur,alloc,from,from2,large,largeRoots);
	  }
	break;
      }
    case IARRAY_TAG:
    case RARRAY_TAG: 
      {
	unsigned int wordLen = (GET_ARRLEN(tag) + 3) / 4; /* IARRAY len might not be mult of 4 */
	if (wordLen == 0)
	  wordLen = 1;
	cur += 1 + wordLen;
	break;
      }
    case PARRAY_TAG:
      {
	unsigned int wordLen = GET_ARRLEN(tag) / 4;
	if (wordLen == 0)
	  cur += 2;
	else {
	    unsigned int *end = cur + 1 + wordLen;
	    cur++;
	    while (cur < end) {
		if (paranoid)
		  check_ptr((ploc_t)cur);
		alloc = forward2((ploc_t)cur,alloc,from,from2,large,largeRoots);
		cur++;
	      }
	  }
	break;
      }
    case SKIP_TAG:
      cur++;
      break;
    case FORWARD_TAG:
    default:
      printf("\n\nscan2_object found bad tag = %d at cur=%d\n",tag,cur);
      assert(0);
    }
  (*where) = cur;
  return alloc;
}


mem_t scan2_region(mem_t start_scan, mem_t alloc_ptr, mem_t stop, 
		   range_t *from_range, range_t *from2_range, range_t *to_range,
		   range_t *large, Queue_t *largeRoots)
{
  mem_t cur = start_scan;
  unsigned int a,b, c=0;

  while (cur < alloc_ptr && cur < stop)
    {
      tag_t tag = cur[0];
      unsigned int type= GET_TYPE(tag);
      if ((GET_TYPE(tag) == RECORD_TAG) && (GET_RECLEN(tag) <= 2))
	{
	  if (tag == TAG_REC_TRACETRACE) {
	    if (paranoid) {
	      check_ptr((ploc_t)cur+1);
	      check_ptr((ploc_t)cur+2);
	    }
	    alloc_ptr = forward2((ploc_t)cur+1,alloc_ptr,from_range,from2_range,large,largeRoots);
	    alloc_ptr = forward2((ploc_t)cur+2,alloc_ptr,from_range,from2_range,large,largeRoots);
	    cur += 3;
	    continue;
	  }
	  else if (tag == TAG_REC_INTTRACE) {
	      alloc_ptr = forward2((ploc_t)cur+2,alloc_ptr,from_range,from2_range,large,largeRoots);
	      cur += 3;
	      continue;
	    }
	  else if (tag == TAG_REC_INT) {
	      cur += 2;
	      continue;
	    }
	  else if (tag == TAG_REC_TRACE) {
	      alloc_ptr = forward2((ploc_t)cur+1,alloc_ptr,from_range,from2_range,large,largeRoots);
	      cur += 2;
	      continue;
	    }
	  else if (tag == TAG_REC_INTINT) {
	      cur += 3;
	      continue;
	    }
	  else if (tag == TAG_REC_TRACEINT) {
	      alloc_ptr = forward2((ploc_t)cur+1,alloc_ptr,from_range,from2_range,large,largeRoots);
	      cur += 3;
	      continue;
	    }
	  else 
	    assert(0);
      }
      else if (GET_TYPE(tag) == SKIP_TAG) {
	  cur++;
	  continue;
	}
      else {
	mem_t temp = cur;
	alloc_ptr = scan2_object(&temp, alloc_ptr, from_range, from2_range, to_range,large,largeRoots);
	cur = temp;
	continue;
      }
    }
  return alloc_ptr;
}



mem_t scan1_until(mem_t start_scan, mem_t alloc_ptr, 
		  range_t *from_range, range_t *to_range)
{
  mem_t cur = start_scan;
  unsigned int a,b, c=0;

  while (cur < alloc_ptr)
    {
      tag_t tag = cur[0];
      unsigned int type = GET_TYPE(tag);

      if ((GET_TYPE(tag) == RECORD_TAG) && (GET_RECLEN(tag) <= 2))
	{
	  if (tag == TAG_REC_TRACETRACE) {
	      alloc_ptr = forward1((ploc_t)cur+1,alloc_ptr,from_range);
	      alloc_ptr = forward1((ploc_t)cur+2,alloc_ptr,from_range);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_INTTRACE) {
	      alloc_ptr = forward1((ploc_t)cur+2,alloc_ptr,from_range);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_INT) {
	      cur += 2;
	    }
	  else if (tag == TAG_REC_TRACE) {
	      alloc_ptr = forward1((ploc_t)cur+1,alloc_ptr,from_range);
	      cur += 2;
	    }
	  else if (tag == TAG_REC_INTINT) {
	      cur += 3;
	    }
	  else if (tag == TAG_REC_TRACEINT) {
	      alloc_ptr = forward1((ploc_t)cur+1,alloc_ptr,from_range);
	      cur += 3;
	    }
	  else 
	    alloc_ptr = scan1_object(&cur, alloc_ptr, from_range, to_range);
      }
      else if (GET_TYPE(tag) == SKIP_TAG)
	cur++;
      else
	alloc_ptr = scan1_object(&cur, alloc_ptr, from_range, to_range);
    }
  assert(cur == alloc_ptr);
  return alloc_ptr;
}


mem_t scan1_region(mem_t start_scan, mem_t alloc, mem_t stop,
		   range_t *from_range, range_t *to_range)
{
  register mem_t cur = start_scan;

  while (cur < stop)
    {
      tag_t tag = cur[0]; /* note that declaration of variable causes a stack slot
			     allocated for it by the C compiler */
      if (tag == SKIP_TAG)
	continue;
      if ((GET_TYPE(tag) == RECORD_TAG) && (GET_RECLEN(tag) <= 2))
	{
	  if (tag == TAG_REC_TRACETRACE) {
	      alloc = forward1((ploc_t)cur+1,alloc,from_range);
	      alloc = forward1((ploc_t)cur+2,alloc,from_range);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_INTTRACE) {
	      alloc = forward1((ploc_t)cur+2,alloc,from_range);
	      cur += 3;
	    }
	  else if (tag == TAG_REC_INT) {
	      cur += 2;
	    }
	  else if (tag == TAG_REC_TRACE) {
	      alloc = forward1((ploc_t)cur+1,alloc,from_range);
	      cur += 2;
	    }
	  else if (tag == TAG_REC_INTINT) {
	      cur += 3;
	    }
	  else if (tag == TAG_REC_TRACEINT) {
	      alloc = forward1((ploc_t)cur+1,alloc,from_range);
	      cur += 3;
	    }
	  else {
	      mem_t temp = cur;
	      alloc = scan1_object(&temp, alloc, from_range, to_range);
	      cur = temp;
	    }
	}
      else if (GET_TYPE(tag) == SKIP_TAG)
	  cur++;
      else 
	{
	  mem_t temp = cur;
	  alloc = scan1_object(&temp, alloc, from_range, to_range);
	  cur = temp;
	}
    }

  return alloc;
}
  
void scan1_object_atomic_stack(ptr_t gray,  mem_t *alloc_ptr, mem_t *limit_ptr, Heap_t *toheap,
			       range_t *from_range,  range_t *to_range, SysThread_t *sysThread)
{
  tag_t tag = gray[-1];
  unsigned int type= GET_TYPE(tag);

  while (tag == stall)
    tag = gray[-1];

  if (tag == SKIP_TAG)
    assert(0);

  switch (type)
    {
    case RECORD_TAG:
      {
	int i, fieldlen = GET_RECLEN(tag);
	loc_t cursor = gray;
	mem_t end = gray + fieldlen;
	unsigned mask = GET_RECMASK(tag);
	for (; cursor<end; cursor++, mask >>= 1) {
	  if (mask & 1)
	    forward1_atomic_stack((ploc_t)cursor,alloc_ptr,limit_ptr,toheap,from_range,sysThread);
	}
	if (mask != 0) {
	  printf("scan_object_minor_stack: bad tag %d\n", tag);
	  assert(0);
	}
	break;
      }
    case IARRAY_TAG:
      break;
    case RARRAY_TAG:
      break;
    case PARRAY_TAG:
      {
	unsigned int len = GET_ARRLEN(tag) / 4, i;
	for (i=0; i<len; i++) 
	  forward1_atomic_stack((ploc_t)gray+i,alloc_ptr,limit_ptr,toheap,from_range,sysThread);
	break;
      }
    case SKIP_TAG:
    case FORWARD_TAG:
    default:
      printf("\n\nScan1_object_atomic_stack impossible: tag = %d at gray=%d\n",tag,gray);
      assert(0);
    }
}


void scan2_object_atomic_stack(ptr_t gray,  mem_t *alloc_ptr, mem_t *limit_ptr, Heap_t *toheap,
			       range_t *from_range,  range_t *from2_range,
			       range_t *large_range, SysThread_t *sysThread)
{
  tag_t tag = gray[-1];
  unsigned int type= GET_TYPE(tag);

  while (tag == stall)
    tag = gray[-1];

  if (tag == SKIP_TAG)
    assert(0);

  switch (type)
    {
    case RECORD_TAG:
      {
	int i, fieldlen = GET_RECLEN(tag);
	loc_t cursor = gray;
	mem_t end = gray + fieldlen;
	unsigned mask = GET_RECMASK(tag);
	for (; cursor<end; cursor++, mask >>= 1) {
	  if (mask & 1)
	    forward2_atomic_stack((ploc_t)cursor,alloc_ptr,limit_ptr,toheap,
				  from_range,from2_range,large_range,sysThread);
	}
	if (mask != 0) {
	  printf("scan_object_minor_stack: bad tag %d\n", tag);
	  assert(0);
	}
	break;
      }
    case IARRAY_TAG:
      break;
    case RARRAY_TAG:
      break;
    case PARRAY_TAG:
      {
	unsigned int len = GET_ARRLEN(tag) / 4, i;
	for (i=0; i<len; i++) 
	  forward2_atomic_stack((ploc_t)gray+i,alloc_ptr,limit_ptr,toheap,
				from_range,from2_range,large_range,sysThread);
	break;
      }
    case SKIP_TAG:
    case FORWARD_TAG:
    default:
      printf("\n\nScan2_object_atomic_stack impossible: tag = %d at gray=%d\n",tag,gray);
      assert(0);
    }
}
