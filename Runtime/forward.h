/* Unfortunately, the DEC cc compiler will not inline functions
   that use 32-bit pointers.  On the other hand, gcc
   on the Alpha does not support 32-bit pointers at all.
*/


#ifndef _forward_h
#define _forward_h

#include "queue.h"
#include "general.h"
#include "memobj.h"
#include "thread.h"
#include "gc_large.h"
#include "show.h"

/* ------------------- Misc Routines ---------------------- */
unsigned long objectLength(ptr_t obj, mem_t *start);  /* Gives the object length including tag words.  A skip tag is also legal input. */
int empty_writelist(Proc_t *proc);
void process_writelist(Proc_t *proc, Heap_t *from, Heap_t *to);

/* ------------------------- Functions relating to CopyRange_t --------------------------- */

void  DischargeCopyRange(CopyRange_t *copyRange);           /* Leftover area is padded with PadHeapArea */
mem_t AllocFromCopyRangeSlow(Proc_t *proc, int size, Align_t align);    /* Leftover area is padded with PadHeapArea */
void  SetCopyRange(CopyRange_t *copyRange, Proc_t *, Heap_t *heap, Set_t *region);
void  PadCopyRange(CopyRange_t *copyRange);                 /* Current unused area is padded with PadHeapArea */
void  ClearCopyRange(CopyRange_t *copyRange);
int   IsNullCopyRange(CopyRange_t *copyRange);
void  AllocEntireCopyRange(CopyRange_t *copyRange);    /* Allocate entire heap to copy range - for uniprocessors so space check is avoided */
void  ReturnCopyRange(CopyRange_t *copyRange);         /* Return remainder of copy range to heap - copy range must reside contiguous to heap */
void AddGrayCopyRange(CopyRange_t *copyRange);

/* When align and noCheck are statically known, the compiler can greatly simplify the function body */
INLINE(allocFromCopyRange)
mem_t allocFromCopyRange(Proc_t *proc, int request, Align_t align, int noCheck) /* request is a multiple of 4 */
{
  mem_t region;
  int alignedRequest = (align == NoWordAlign) ? request : 4 + request;
  CopyRange_t *copyRange = &proc->copyRange;

  if (noCheck) {
    copyRange->cursor = AlignMemoryPointer(copyRange->cursor,align);
    region = copyRange->cursor;
    copyRange->cursor += (request / sizeof(val_t));
  }
  else {
    if (copyRange->cursor + (alignedRequest / sizeof(val_t)) < copyRange->stop) {
      copyRange->cursor = AlignMemoryPointer(copyRange->cursor,align);
      region = copyRange->cursor;
      copyRange->cursor += (request / sizeof(val_t));
    }
    else
      region = AllocFromCopyRangeSlow(proc, request, align);
  }
#ifdef sparc
  if (0) {
    int discard;
    int next = ((int) region) + 64; /* Fetch next block */
    asm("prefetch [%1], 2" : "=r" (discard) : "r" (next));
  }
#endif

  return region;
}


/* A few definition
   -----------------
   Note that "copy" = "alloc" + "transfer"

   transferScanObj_* - copy the fields of the given primary object to the replica object indicated by the forwarding pointer;
                       apply a function * to each pointer field of the object; returns nothing
   scanObj_*         - apply a function * to each pointer field of the object; returns nothing
   scanTag_*         - apply a function * to each pointer field of the object just after the tag; returning address just past obj
   scanUntil_*       - apply a function * to each pointer field of the objects until the allocation pointer equals the scan pointer
   alloc_*           - alloc space for a copy of the object and install a forwarding pointer in the original object but do not copy fields
   copy_*            - makes a copy of an object and install a forwarding pointer in the original object
   locCopy_*         - update a location containing a pointer to an object with a pointer to the copy;
                       this usually involves calling the copy_* version of the function
   locAlloc_*        - update a location containing a pointer to an object with a pointer to the copy;
                       this usually involves calling the alloc_* version of the function
   noSpaceCheck_     - assume there is enough room in the CopyRange; this is possible in uniprocessor stop-and-copy  collection
   copyCopySync_*    - a modifier on copy/alloc; the object is locked by atomically installing STALL_TAG; to synchronize multiple copiers 
   scanWriteSync_*   - a modifier on scan; the object is reserved by writing STALL_TAG; to synchronize a copier and a writer
   primarySet_*      - a copy/alloc routine that inserts the primary into the set if the primary was actually copied
   replicaSet_*      - a copy/alloc routine that inserts the replica into the setk if the primary was actually copied
   1_                - qualifying the copy/alloc by checking if the pointer is in the given source space
   2L_               - qualifying the copy/alloc by checking if the pointer is in one of 2 source spaces;
                       if object is in large space then call gc_large_addRoot
   unique_           - assumes object has not not been copied yet and that there are no other copiers
*/


/* --------------- Functions for copying or allocating space for a copy of an object ----------- */
/* copy_* and alloc_*
   (1) Assumes obj is a heap pointer
   (2) Takes a primary object and (if it is not already copied)
       (a) allocate space for a new copy of the object,
       (b) copy the tag and the fields (if copy and not alloc)
       (c) install a forwarding pointer from the primary to the copy.
   (3) The allocation pointer and limit pointer are updated in copyRange.
   (4) The number of bytes copied is stored in proc->bytesCopied (0 if not copied). proc->segUsage will also be updated.
   (5) The (possibly previously) copied object is returned.
   (6) When _copyCopySync_ is present, the object being copied is locked first to prevent 
       multiple copies from being made because of multiple copiers.  For copy (not alloc), 
       the object is copied coarsely, all fields at once, even if the object is large.
   (7) When _noSpaceCheck_ is present, the limit check on the copyRange is omitted.
   (8) When _primarySet is present, the original object, if just copied, is added to the set
       When _replicaSet is present, the new object, if a copy was just made, is added to the set
*/




/* ----------- Functions relating to copying an object or allocating space for a copy ------------------- */

INLINE(acquireOwnership)
tag_t acquireOwnership(Proc_t *proc, ptr_t white, tag_t tag)
     /* Tag is the the tag value when the caller checked.  It may have changed */
{

#ifdef alpha_osf
  int done = 0;
  while (!done) {
    /*    asm("ldl_l %0,-4(%1)" : "=i" (tag) : "i" (white)); */
    tag = asm("ldl_l %v0,-4(%a0)",white); 
    if (tag == STALL_TAG)
      done = 1;
    else 
      done |= asm("stl_c %a0,-4(%a1) ; mov %a0,%v0",STALL_TAG,white);
  }
  while (tag == STALL_TAG)
    tag = white[-1];
  return tag;
#endif

#ifdef sparc
  mem_t tagloc = white - 1;
  if (tag == STALL_TAG) {         /* Somebody grabbed it but did not finish forwarding */
    while (tag == STALL_TAG) {
      tag = white[-1];
      memBarrier();               /* Might need to refetch from memory */
    }
    assert(TAG_IS_FORWARD(tag));  /* Object forwarded by someone else now */
  }
  else {                          /* Try to be the copier */
    /* Example of a SPARC ld statement with gcc asm
       int *ptr;
       int val;
       asm("ld   [%1],%0" : "=r" (val) : "r" (ptr)); 
       
       The following tries to atomicaly swap in the stall tag by comparing with original tag.
       Note that registers that are input and output are specified twice with the input
       use referring to the output register.
    */
    val_t localStall = STALL_TAG;
    asm("cas [%2],%3,%0" : "=r" (localStall) : "0" (localStall), "r" (tagloc), "r" (tag)); 
    /* localStall == tag           : we are the copier
       localStall == STALL_TAG     : somebody else is the copier and was in the middle of its operation
       localStall == a forward ptr : somebody else is the copier and forwarded it already */
    if (localStall == tag)
      ;                             
    else if (localStall == STALL_TAG) {
      proc->numContention++;
      if (diag) 
	printf("Proc %d: contention copying object white = %d\n", proc->procid, white);
      while ((tag = white[-1]) == STALL_TAG)
	memBarrier();
      assert(TAG_IS_FORWARD(tag));
    }
    else if (TAG_IS_FORWARD(localStall))
      tag = localStall;
    else {
      printf("Proc %d: forward.c: Odd tag of %d from white obj %d with original tag = %d -----------\n", 
	     proc->procid, localStall, white, tag);
      assert(0);
    }
  }
  return tag;
#endif
}


/* Returns the copied/allocated version 
   (1) Check proc->bytesCopied to see if actually copied 
   (2) Check proc->needScan (in addition to bytesCopied) for non-zero to see if copied object might have pointer field
*/
INLINE(genericAlloc)
ptr_t genericAlloc(Proc_t *proc, ptr_t white, int doCopy, 
		   int doCopyCopy, int skipSpaceCheck)
{
  ptr_t obj;                       /* forwarded object */
  tag_t tag = white[-1];           /* original tag */
  int type;

  /* assert(white < copyRange->start || white >= copyRange->stop); */

  /* If the objects has not been forwarded, atomically try commiting to be the copier.
     When we leave the block, we are the copier if "tag" is not a forwarding pointer. */
  if (TAG_IS_FORWARD(tag)) {
     ptr_t replica = (ptr_t) tag;
     fastAssert(replica != (ptr_t) replica[-1]); /* Make sure object is not self-forwarded */
     proc->numShared++;
     proc->bytesCopied = 0;
     return replica;
  }
  else {
    if (doCopyCopy && doCopyCopySync)  { /* We omit copy-copy sync for measuring the costs of the copy-copy sync */
      tag = acquireOwnership(proc, white, tag);
    }
  }

  proc->segUsage.objsCopied++;

  /* The tag must be restored only after the forwarding address is written */
  type = GET_TYPE(tag);
  if (type == RECORD_TYPE) {           /* As usual, the record case is the most common */
    int i, numFields = GET_RECLEN(tag);
    int objByteLen = 4 * (1 + numFields);
    mem_t region = allocFromCopyRange(proc, objByteLen, NoWordAlign, skipSpaceCheck);
    obj = region + 1;
    if (doCopy)
      for (i=0; i<numFields; i++) {    /* Copy fields */
	obj[i] = white[i];
	}
    obj[-1] = tag;                    /* Write tag last */
    white[-1] = (val_t) obj;        /* Store forwarding pointer last */
    /* Sparc TSO order guarantees forwarding pointer will be visible only after fields are visible */
    proc->numCopied++;
    proc->segUsage.fieldsCopied += numFields;
    proc->bytesCopied = objByteLen;
    proc->needScan = GET_RECMASK(tag);
    return obj;
  }
  else if (TYPE_IS_FORWARD(type)) {
    ptr_t replica = (ptr_t) tag;
    fastAssert(replica != (ptr_t) replica[-1]); /* Make sure object is not self-forwarded */
    proc->numShared++;
    proc->bytesCopied = 0;
    return replica;
  }
  else if (TYPE_IS_ARRAY(type)) {
    int i, arrayByteLen = GET_ANY_ARRAY_LEN(tag);
    int dataByteLen = RoundUp(arrayByteLen, 4);
    int numTags = 1 + ((arraySegmentSize > 0) ? 
		       (arrayByteLen > arraySegmentSize ? DivideUp(arrayByteLen,arraySegmentSize) : 0) : 0);
    int objByteLen = dataByteLen + 4 * numTags;
    Align_t align = (type == QUAD_ARRAY_TYPE) ? ((numTags & 1) ? OddWordAlign : EvenWordAlign) : NoWordAlign;
    mem_t region = allocFromCopyRange(proc, objByteLen, align, skipSpaceCheck);
    obj = region + numTags;
    if (type != WORD_ARRAY_TYPE)
      assert(arrayByteLen % 4 == 0);
    if (doCopy)
      memcpy((char *) obj, (const char *)white, objByteLen - 4);
    for (i=0; i<numTags-1; i++)
      obj[-2-i] = SEGPROCEED_TAG;
    obj[-1] = tag;	
    white[-1] = (val_t) obj;
    proc->numCopied++;
    proc->segUsage.fieldsCopied += objByteLen / 2;
    proc->bytesCopied = objByteLen;
    proc->needScan = (type == PTR_ARRAY_TYPE || type == MIRROR_PTR_ARRAY_TYPE);
    return obj;
  }

  printf("\n\nError in genericAlloc: bad tag value %d of white object %d\n",tag, white);
  memdump("", white - 8, 16, white - 1);

  assert(0);
}


static ptr_t copy_replicaSet(Proc_t *proc, ptr_t white)
{
  ptr_t replica = genericAlloc(proc, white, 1, 0, 0);
  if (proc->bytesCopied) {
    SetPush(&proc->work.objs, (ptr_t) white[-1]);
  }
  return replica;
}


ptr_t copy(Proc_t *, ptr_t obj);
ptr_t alloc(Proc_t *, ptr_t obj);
ptr_t copy_noSpaceCheck(Proc_t *, ptr_t obj);
ptr_t copy_noSpaceCheck_replicaSet(Proc_t *, ptr_t obj);
ptr_t copy_copyCopySync(Proc_t *, ptr_t obj);
ptr_t copy_copyCopySync_primarySet(Proc_t *, ptr_t obj);
ptr_t copy_copyCopySync_replicaSet(Proc_t *, ptr_t obj);
ptr_t alloc_copyCopySync(Proc_t *, ptr_t obj);
ptr_t alloc_copyCopySync_primarySet(Proc_t *, ptr_t obj);
ptr_t alloc_copyCopySync_replicaSet(Proc_t *, ptr_t obj);
ptr_t copy_noSpaceCheck_copyCopySync(Proc_t *, ptr_t obj);
ptr_t copy_noSpaceCheck_copyCopySync_replicaSet(Proc_t *, ptr_t obj);



/* ---------  Conditional copy functions ------------------ */
/* A 1 indicates that the object is copied if it is in the designated range.
   A 2 indicates that the object is copied if it is in one of the 2 designated range.
   The addition of L to 1 or 2 indicates that the object, if in the large object range, is added to largeRoots. 
*/


INLINE(copy1_noSpaceCheck)
void copy1_noSpaceCheck(Proc_t *proc, ptr_t obj, Heap_t *from)
{
  if (InRange((mem_t) obj, &from->range))
    copy_noSpaceCheck(proc,obj);
}

INLINE(locCopy1_noSpaceCheck)
void locCopy1_noSpaceCheck(Proc_t *proc, ploc_t loc, Heap_t *from)
{
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range)) {
    *loc = copy_noSpaceCheck(proc,obj);
  }
}

INLINE(locCopy1)
void locCopy1(Proc_t *proc, ploc_t loc, Heap_t *from)
{
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range)) {
    *loc = copy(proc,obj);
  }
}

INLINE(locCopy2L_noSpaceCheck)
void locCopy2L_noSpaceCheck(Proc_t *proc, ploc_t loc,
			    Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range) ||
      InRange((mem_t) obj, &from2->range)) {
    *loc = copy_noSpaceCheck(proc,obj);
  }
  else if ((val_t) obj - (val_t) large->range.low < large->range.diff)
    gc_large_addRoot(obj);
}

INLINE(copy2L_noSpaceCheck)
void copy2L_noSpaceCheck(Proc_t *proc, ptr_t obj, 
			 Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  if (InRange((mem_t) obj, &from->range) ||
      InRange((mem_t) obj, &from2->range)) {
    copy_noSpaceCheck(proc,obj);
  }
  else if ((val_t) obj - (val_t) large->range.low < large->range.diff)
    gc_large_addRoot(obj);
}

INLINE(copy1_copyCopySync_replicaSet)
void copy1_copyCopySync_replicaSet(Proc_t *proc, ptr_t obj, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    copy_copyCopySync_replicaSet(proc,obj);
}

INLINE(alloc1_copyCopySync_primarySet)
void alloc1_copyCopySync_primarySet(Proc_t *proc, ptr_t obj, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_primarySet(proc,obj);
}

INLINE(alloc1L_copyCopySync_primarySet)
void alloc1L_copyCopySync_primarySet(Proc_t *proc, ptr_t obj, Heap_t *from, Heap_t *large)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_primarySet(proc,obj);
  else if (InRange((mem_t) obj, &large->range)) 
    gc_large_addRoot(obj);
}

INLINE(locAlloc1_copyCopySync_primarySet)
void locAlloc1_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = alloc_copyCopySync_primarySet(proc,white);
  }
}

INLINE(locAlloc1L_copyCopySync_primarySet)
void locAlloc1L_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from, Heap_t *large)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = alloc_copyCopySync_primarySet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range)) 
    gc_large_addRoot(white);
}

INLINE(locCopy1_copyCopySync)
void locCopy1_copyCopySync(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_copyCopySync(proc,white);
  }
}

INLINE(locCopy1_copyCopySync_replicaSet)
void locCopy1_copyCopySync_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_copyCopySync_replicaSet(proc,white);
  }
}

INLINE(locCopy1_replicaSet)
void locCopy1_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_replicaSet(proc,white);
  }
}

INLINE(locCopy1_noSpaceCheck_copyCopySync_replicaSet)
void locCopy1_noSpaceCheck_copyCopySync_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;						
  if (InRange((mem_t) white, &from->range)) {	
    *loc = copy_noSpaceCheck_copyCopySync_replicaSet(proc,white);
  }
}

INLINE(locCopy1_noSpaceCheck_replicaSet)
void locCopy1_noSpaceCheck_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_noSpaceCheck_replicaSet(proc,white);
  }
}


INLINE(locCopy1_noSpaceCheck_copyCopySync)
void locCopy1_noSpaceCheck_copyCopySync(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range)) {				
    *loc = copy_noSpaceCheck_copyCopySync(proc,white);
  }
}

INLINE(copy1_copyCopySync_primarySet)
void copy1_copyCopySync_primarySet(Proc_t *proc, ptr_t white, Heap_t *from)
{ 
  if (InRange((mem_t) white, &from->range)) 
    copy_copyCopySync_primarySet(proc,white);
}

INLINE(locCopy1_copyCopySync_primarySet)
void locCopy1_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = copy_copyCopySync_primarySet(proc,white);
  }
}

INLINE(locCopy1L_copyCopySync_primarySet)
void locCopy1L_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_copyCopySync_primarySet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range)) 
    gc_large_addRoot(white);
}

INLINE(locCopy2L_copyCopySync_replicaSet)
void locCopy2L_copyCopySync_replicaSet(Proc_t *proc, ploc_t loc, 
				       Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    *loc = copy_copyCopySync_replicaSet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

INLINE(copy2L_copyCopySync_primarySet)
void copy2L_copyCopySync_primarySet(Proc_t *proc, ptr_t white, 
				      Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) 
    copy_copyCopySync_primarySet(proc,white);
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

INLINE(locCopy2L_copyCopySync_primarySet)
void locCopy2L_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, 
					 Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    *loc = copy_copyCopySync_primarySet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}


INLINE(locCopy2L_copyCopySync_replicaSet)
void locCopy2L_copyCopySync_replica(Proc_t *proc, ploc_t loc,
				    Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    *loc = copy_copyCopySync_replicaSet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

INLINE(locAlloc2L_copyCopySync_primarySet)
void locAlloc2L_copyCopySync_primarySet(Proc_t *proc, ploc_t loc,
					  Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    *loc = alloc_copyCopySync_primarySet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

#include "scan.h"

#endif
