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


void dischargeWithPad(CopyRange_t *copyRange);           /* Leftover area is padded with PadHeapArea */
void expandWithPad(CopyRange_t *copyRange, int size);    /* Leftover area is padded with PadHeapArea */

void SetCopyRange(CopyRange_t *copyRange, Proc_t *, Heap_t *heap, expand_t *expand, discharge_t *discharge);
void ClearCopyRange(CopyRange_t *copyRange);

unsigned long objectLength(ptr_t obj);

/* getNontagNonglobalPointerLocations - used for processing globals
   (1) Takes an (initilaized) object and a queue of pointer locations
   (2) Decodes the object using its tag and place into the queue 
       all the pointer fields that do not contain
       tags (small constructor values) or addresses of globals
   (3) Return the number of locations added. */

int getNontagNonglobalPointerLocations(ptr_t obj, Queue_t *locs);


/* A few definitions:
   scan_obj_*        - to apply a function * to each pointer field of the object
   scan_area_*       - to apply a function * to each pointer field of the objects in the area
   copy_*            - to make a copy of an object and install a forwarding pointer in the original object
   forward_*         - updates a location containing a pointer to an object with a poniter to the copy;
                       this may involve calling the copy_* version of the function
   coarseParallel_* - to copy/forward an object by locking down the object so that multiple copiers 
                       are synchronized
   stack_*           - a copy/forward routine that inserts the copied object into a stack if the 
                       primary was actually copied
*/


/* forward - don't call this directly 
   (1) Takes a location containing the primary object,
       unconditionaly creates a copy of the object,
       install a forwarding pointer from the primary to the copy, 
       and updates the location with the copy.
   (2) The updated allocation pointer is returned.
*/
mem_t forward(Proc_t *, ploc_t vpp, mem_t alloc);  


/* copy_coarseParallel - don't call this directly
   (1) Takes a primary object,
       unconditionaly creates a copy of the object,
       install a forwarding pointer from the primary to the copy.
   (2) The allocation pointer and limit pointer are passed by reference
       and are updated by allocating new areas from the heap parameter.
   (3) The object being copied is locked first to prevent multiple copies
       from being made because of multiple copiers.  The object is 
       copied coarsely, all at once, even if the object is large.
   (4) Returns the number of bytes copied.
*/
int copy_coarseParallel(Proc_t *, ptr_t obj, CopyRange_t *copyRange);


/* forward_coarseParallel - don't call this directly
   (1) Same as copy_coarseParallel except that the primary object is not passed.
       Instead, a location containing the object is taken as input.
       Further, the location will be updated with the copy.
*/
int forward_coarseParallel(Proc_t *, ploc_t vpp, CopyRange_t *copyRange);


/* forward1
   (1) Calls the underlying forward routine after checking
       that the pointer at the given location is in from-space */
#ifdef alpha_osf
static        mem_t forward1(Proc_t *sth, ploc_t vpp, mem_t alloc, range_t *from)                       
#pragma inline forward1
#else
static inline mem_t forward1(Proc_t *sth, ploc_t vpp, mem_t alloc, range_t *from)                       
#endif
{
  if ((val_t)(*vpp) - (val_t)from->low < from->diff)
    alloc = forward(sth,vpp,alloc);                       
  return alloc;
}

/* forward2
   (1) Calls the underlying forward routine after checking
       that the pointer at the given location is in one of two from-spaces 
   (2) If pointer is in the large object range, then add it
       to the queue containing large object roots */
#ifdef alpha_osf
static        mem_t forward2(Proc_t *sth, ploc_t vpp, mem_t alloc, 
#pragma inline forward2
#else
static inline mem_t forward2(Proc_t *sth, ploc_t vpp, mem_t alloc, 
#endif
			     range_t *from, range_t *from2,
			     range_t *large, Queue_t *largeRoots)
{ 
  ptr_t p = *vpp;                                      
  if (((val_t) p - (val_t) from->low < from->diff) ||  
      ((val_t) p - (val_t) from2->low < from2->diff))  
    alloc = forward(sth,vpp,alloc);                        
  else if ((val_t) p - (val_t) large->low < large->diff)
    Enqueue(largeRoots, p);
  return alloc;
}


/* forward1_coarseParallel_stack
   (1) Calls the underlying forward_coarseParallel routine after
       checking that the pointer at the given location is in the from-space.
   (2) If the object was actually forwarded, the (newly-made) 
       forwarded object is inserted into the system thread's stack. */
#ifdef alpha_osf
static        int forward1_coarseParallel_stack(ploc_t vpp, CopyRange_t *copyRange, range_t *from, Proc_t *proc)
#pragma inline forward1_coarseParallel_stack
#else
static inline int forward1_coarseParallel_stack(ploc_t vpp, CopyRange_t *copyRange, range_t *from, Proc_t *proc)
#endif
{ 
  ptr_t p = *vpp;							
  if ((val_t) p - (val_t)from->low < from->diff) {
    int bytesCopied = forward_coarseParallel(proc,vpp,copyRange);
    assert(*vpp != p);
    if (bytesCopied) {
      proc->localStack.stack[proc->localStack.cursor++] = (loc_t)(*vpp);
      assert(proc->localStack.cursor < (sizeof(proc->localStack.stack) / sizeof (ptr_t)));
      return bytesCopied;
    }
  }
  return 0;
}

/* forward2_coarseParallel_stack
   (1) Calls the underlying forward_coarseParallel routine after
       checking that the pointer at the given location is in the from-space.
   (2) If the object was actually forwarded, the (newly-made) 
       forwarded object is inserted into the system thread's stack. 
   (3) If pointer is in the large object range, then add it
       to the queue containing large object roots 
*/
#ifdef alpha_osf
static         int forward2_coarseParallel_stack(ploc_t vpp, CopyRange_t *copyRange,
						range_t *from, range_t *from2, range_t *large, 
						Proc_t *proc)
#pragma inline forward2_coarseParallel_stack
#else
static inline int forward2_coarseParallel_stack(ploc_t vpp, CopyRange_t *copyRange,
						range_t *from, range_t *from2, range_t *large, 
						Proc_t *proc)
#endif
{ 
  ptr_t p = *vpp;							 	    
  if (((val_t) p - (val_t)from->low < from->diff) ||
      ((val_t)  p - (val_t)from2->low < from2->diff)) {
    int bytesCopied = forward_coarseParallel(proc,vpp,copyRange);
    assert(*vpp != p);
    if (bytesCopied) {
      proc->localStack.stack[proc->localStack.cursor++] = (loc_t)(*vpp);
      assert(proc->localStack.cursor < (sizeof(proc->localStack.stack) / sizeof (ptr_t)));
    }
    return bytesCopied;
  }
  else if ((val_t) p - (val_t) large->low < large->diff)
    Enqueue(proc->largeRoots, p);
  return 0;
}

/* forward1_concurrent_stack
   (1) Calls the underlying forward_coarseParallel routine after
       checking that the pointer at the given location is
       in the from-space.
   (2) If the object was actually forwarded, the (newly-made) 
       forwarded object is inserted into the system thread's stack. */
#ifdef alpha_osf
static        int forward1_concurrent_stack(ptr_t p, CopyRange_t *copyRange, range_t *from, Proc_t *proc)
#pragma inline forward1_concurrent_stack
#else
static inline int forward1_concurrent_stack(ptr_t p, CopyRange_t *copyRange, range_t *from, Proc_t *proc)
#endif
{ 
  if ((val_t) p - (val_t)from->low < from->diff) {
    int bytesCopied = copy_coarseParallel(proc,p,copyRange);
    assert((ptr_t) p[-1] != p);
    if (bytesCopied) {
      proc->localStack.stack[proc->localStack.cursor++] = (loc_t)(p[-1]);
      assert(proc->localStack.cursor < (sizeof(proc->localStack.stack) / sizeof (ptr_t)));
    }
    return bytesCopied;
  }
  return 0;
}


/* forward1_root_lists, forward2_root_lists
   (1) Repeatedly call forward1/forward2 on each given root.
   (2) The root_lists argument is a queue of queue of roots (ploc_t)
   (3) The updated allocation pointer is returned.
*/
mem_t forward1_root_lists(Proc_t *, Queue_t *root_lists, mem_t alloc,
			  range_t *from_range, range_t *to_range);
mem_t forward2_root_lists(Proc_t *, Queue_t *root_lists, mem_t alloc,
			  range_t *from_range, range_t *from2_range, range_t *to_range,
			  range_t *large, Queue_t *largeRoots);

/* forward1_writelist, forward1_writelist_coarseParallel
   (1) Call forward1 or forward1_coarseParallel on each location in the writelist
   (2) the writelist is terminated by a NULL 
   (3) The updated allocation pointer is returned. 
*/
void discard_writelist(Proc_t *proc);
void forward1_writelist_coarseParallel_stack(CopyRange_t *copyRange, range_t *from, range_t *to, Proc_t *proc);

mem_t forward1_writelist(Proc_t *proc, mem_t alloc,
			 range_t *from, range_t *to);



/* ------------------- Scanning Routines ---------------------- */

/* scan1_region
   (1) Scan the objects from start_scan to stop
   (2) For each object, forward_minor its pointer fields (ploc_t)
   (3) Return the new allocation pointer. */
mem_t scan1_region(Proc_t *, mem_t start_scan, mem_t alloc, mem_t stop,
		   range_t *from_range, range_t *to_range);

/* scan1_until - standard cheney scan
   (1) Scan the objects from start_scan until there are no more objects.
       That is, keep scanning until we hit the allocation pointer.
   (2) For each object, forward_minor its pointer fields (ploc_t)
   (3) Return the new allocation pointer. */
mem_t scan1_until(Proc_t *, mem_t start_scan, mem_t alloc,
		  range_t *from_range, range_t *to_range);

/* scan2_region
   (1) Scan the objects from start_scan to stop.
   (2) For each object, forward2 its pointer fields (ploc_t)
   (3) Return the new allocation pointer. */
mem_t scan2_region(Proc_t *, mem_t start_scan, mem_t alloc, mem_t stop, 
		   range_t *from_range, range_t *from2_range, range_t *to_range,
		   range_t *large, Queue_t *largeRoots);

/* scan1_object_coarseParallel_stack, scan2_object_coarseParallel_stack
   (1) Scan the given object, calling forward1_coarseParallel or forward2_coarseParallel
       on each pointer field.
   (2) The allocation/limit pointers and stack may be updated.
   (3) Returns the size of the object
*/
int scan1_object_coarseParallel_stack(ptr_t gray_obj, CopyRange_t *copyRange,
				       range_t *from_range, range_t *to_range, Proc_t *proc,
				       int *bytesCopied);
int scan2_object_coarseParallel_stack(ptr_t gray_obj, CopyRange_t *copyRange,
				       range_t *from_range, range_t *from2_range,
				       range_t *to_range, Proc_t *proc,
				       int *bytesCopied);

#endif


