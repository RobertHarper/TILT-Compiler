/* Unfortunately, the DEC cc compiler will not INLINE functions
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
void InitCopyRange(CopyRange_t* copyRange, Proc_t* proc, Heap_t* heap);
void  SetCopyRange(CopyRange_t *copyRange, Proc_t *, Heap_t *heap, Set_t *region);
void  PadCopyRange(CopyRange_t *copyRange);                 /* Current unused area is padded with PadHeapArea */
void  ClearCopyRange(CopyRange_t *copyRange);
int   IsNullCopyRange(CopyRange_t *copyRange);
void  AllocEntireCopyRange(CopyRange_t *copyRange);    /* Allocate entire heap to copy range - for uniprocessors so space check is avoided */
void  ReturnCopyRange(CopyRange_t *copyRange);         /* Return remainder of copy range to heap - copy range must reside contiguous to heap */
void AddGrayCopyRange(CopyRange_t *copyRange);

/* When align and noCheck are statically known, the compiler can greatly simplify the function body */
#pragma INLINEP(allocFromCopyRange)
static INLINE
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







ptr_t copy(Proc_t *, ptr_t obj);
ptr_t alloc(Proc_t *, ptr_t obj);
ptr_t copy_replicaSet(Proc_t *, ptr_t obj);
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


#pragma INLINEP(copy1_noSpaceCheck)
static INLINE
void copy1_noSpaceCheck(Proc_t *proc, ptr_t obj, Heap_t *from)
{
  if (InRange((mem_t) obj, &from->range))
    copy_noSpaceCheck(proc,obj);
}

#pragma INLINEP(locCopy1_noSpaceCheck)
static INLINE
void locCopy1_noSpaceCheck(Proc_t *proc, ploc_t loc, Heap_t *from)
{
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range)) {
    *loc = copy_noSpaceCheck(proc,obj);
  }
}

#pragma INLINEP(locCopy1)
static INLINE
void locCopy1(Proc_t *proc, ploc_t loc, Heap_t *from)
{
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range)) {
    *loc = copy(proc,obj);
  }
}

#pragma INLINEP(locCopy2L_noSpaceCheck)
static INLINE
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

#pragma INLINEP(copy2L_noSpaceCheck)
static INLINE
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

#pragma INLINEP(copy1_copyCopySync_replicaSet)
static INLINE
void copy1_copyCopySync_replicaSet(Proc_t *proc, ptr_t obj, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    copy_copyCopySync_replicaSet(proc,obj);
}

#pragma INLINEP(alloc1_copyCopySync_primarySet)
static INLINE
void alloc1_copyCopySync_primarySet(Proc_t *proc, ptr_t obj, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_primarySet(proc,obj);
}

#pragma INLINEP(alloc1_copyCopySync)
static INLINE
void alloc1_copyCopySync(Proc_t *proc, ptr_t obj, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync(proc,obj);
}

#pragma INLINEP(alloc1_copyCopySync_replicaSet)
static INLINE
void alloc1_copyCopySync_replicaSet(Proc_t *proc, ptr_t obj, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_replicaSet(proc,obj);
}

#pragma INLINEP(alloc1L_copyCopySync_primarySet)
static INLINE
void alloc1L_copyCopySync_primarySet(Proc_t *proc, ptr_t obj, Heap_t *from, Heap_t *large)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_primarySet(proc,obj);
  else if (InRange((mem_t) obj, &large->range)) 
    gc_large_addRoot(obj);
}

#pragma INLINEP(locAlloc1_copyCopySync_primarySet)
static INLINE
void locAlloc1_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = alloc_copyCopySync_primarySet(proc,white);
  }
}


#pragma INLINEP(locAlloc1_copyCopySync)
static INLINE
void locAlloc1_copyCopySync(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = alloc_copyCopySync(proc,white);
  }
}

#pragma INLINEP(locAlloc1_copyCopySync_replicaSet)
static INLINE
void locAlloc1_copyCopySync_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = alloc_copyCopySync_replicaSet(proc,white);
  }
}

#pragma INLINEP(locAlloc1L_copyCopySync_primarySet)
static INLINE
void locAlloc1L_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from, Heap_t *large)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = alloc_copyCopySync_primarySet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range)) 
    gc_large_addRoot(white);
}

#pragma INLINEP(locCopy1_copyCopySync)
static INLINE
void locCopy1_copyCopySync(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_copyCopySync(proc,white);
  }
}

#pragma INLINEP(locCopy1_copyCopySync_replicaSet)
static INLINE
void locCopy1_copyCopySync_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_copyCopySync_replicaSet(proc,white);
  }
}

#pragma INLINEP(locCopy1_replicaSet)
static INLINE
void locCopy1_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_replicaSet(proc,white);
  }
}

#pragma INLINEP(locCopy1_noSpaceCheck_copyCopySync_replicaSet)
static INLINE
void locCopy1_noSpaceCheck_copyCopySync_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;						
  if (InRange((mem_t) white, &from->range)) {	
    *loc = copy_noSpaceCheck_copyCopySync_replicaSet(proc,white);
  }
}

#pragma INLINEP(locCopy1_noSpaceCheck_replicaSet)
static INLINE
void locCopy1_noSpaceCheck_replicaSet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_noSpaceCheck_replicaSet(proc,white);
  }
}


#pragma INLINEP(locCopy1_noSpaceCheck_copyCopySync)
static INLINE
void locCopy1_noSpaceCheck_copyCopySync(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range)) {				
    *loc = copy_noSpaceCheck_copyCopySync(proc,white);
  }
}

#pragma INLINEP(copy1_copyCopySync_primarySet)
static INLINE
void copy1_copyCopySync_primarySet(Proc_t *proc, ptr_t white, Heap_t *from)
{ 
  if (InRange((mem_t) white, &from->range)) 
    copy_copyCopySync_primarySet(proc,white);
}

#pragma INLINEP(locCopy1_copyCopySync_primarySet)
static INLINE
void locCopy1_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    *loc = copy_copyCopySync_primarySet(proc,white);
  }
}

#pragma INLINEP(locCopy1L_copyCopySync_primarySet)
static INLINE
void locCopy1L_copyCopySync_primarySet(Proc_t *proc, ploc_t loc, Heap_t *from, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range)) {			
    *loc = copy_copyCopySync_primarySet(proc,white);
  }
  else if (InRange((mem_t) white, &large->range)) 
    gc_large_addRoot(white);
}

#pragma INLINEP(locCopy2L_copyCopySync_replicaSet)
static INLINE
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

#pragma INLINEP(copy2L_copyCopySync_primarySet)
static INLINE
void copy2L_copyCopySync_primarySet(Proc_t *proc, ptr_t white, 
				      Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) 
    copy_copyCopySync_primarySet(proc,white);
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

#pragma INLINEP(locCopy2L_copyCopySync_primarySet)
static INLINE
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


#pragma INLINEP(locCopy2L_copyCopySync_replicaSet)
static INLINE
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

#pragma INLINEP(locAlloc2L_copyCopySync_primarySet)
static INLINE
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
