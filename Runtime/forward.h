#ifndef _forward_h
#define _forward_h

#include "memobj.h"
#include "thread.h"

struct range_st
{
  mem_t low;            /* pointers */
  mem_t high;
  unsigned int diff;    /* difference in bytes = 4 * (high - low) */
};

typedef struct range_st range_t;

void SetRange(range_t *range, mem_t low, mem_t high);

static inline InRange(mem_t addr, range_t *range) 
{
  return ((unsigned int)addr - (unsigned int)range->low <= range->diff);
}

static inline NotInRange(mem_t addr, range_t *range) 
{
  return ((unsigned int)addr - (unsigned int)range->low > range->diff);
}


/* getNontagPointerLocations - used for processing globals
   (1) Takes an (initilaized) object and a queue of pointer locations
   (2) Decodes the object using its tag and place all the pointer fields 
       not containing tags (small constructor values) in the queue.
   (3) Return the number of locations added. */

int getNontagPointerLocations(ptr_t obj, Queue_t *locs);


/* forward - don't call this directly 
   (1) This routine does NOT check if object is already in tospace
   (2) Takes the address of a pointer to an object (of any color)
       and the an allocation pointer
   (3) Copies the object, if it is not already forwarded, 
       to a newly allocated area using the allocation pointer
   (4) Updates the address with the new pointer (new copy or previously forwarded copy)
   (5) Returns the possibly updated allocation pointer */

mem_t forward(ploc_t vpp, mem_t alloc);  


/* forward_coarse_parallel - don't call this directly
   (1) This routine assumes passed object is NOT already in tospace.
   (2) Takes the address of a pointer to an object,
       the addresses of the allocation and limit pointers, and the tospace.
   (3) Coarsely/atomically copies the object, if it is not already forwarded,
       to a newly allocated area using the allocation pointer.
       If no space is available between the allocation and limit pointers,
       then new space is acquired from the tospace and the alloc/limit
       pointers are updated accordingly.
   (4) Updates the address with the new pointer (new copy or previously forwarded copy)
   (5) Returns a bool indicating if the object was actually forwarded.
       Note that the allocation/limit pointers are passed by reference and
       may be updated. */

int forward_coarse_parallel(ploc_t vpp, mem_t *alloc, mem_t *limit, Heap_t *toheap);  


/* forward1
   (1) Calls the underlying forward routine after checking
       that the pointer at the given location is in from-space */
static inline mem_t forward1(ploc_t vpp, mem_t alloc, range_t *from)                       
{
  if ((val_t)(*vpp) - (val_t)from->low < from->diff)           
    alloc = forward(vpp,alloc);                          
  return alloc;
}

/* forward2
   (1) Calls the underlying forward routine after checking
       that the pointer at the given location is in one of 
       two from-spaces 
   (2) If pointer is in the large object range, then add it
       to the queue containing large object roots */
static inline mem_t forward2(ploc_t vpp, mem_t alloc, 
			     range_t *from, range_t *from2,
			     range_t *large, Queue_t *largeRoots)
{ 
  ptr_t p = *vpp;                                      
  if (((val_t) p - (val_t) from->low < from->diff) ||  
      ((val_t) p - (val_t) from2->low < from2->diff))  
    alloc = forward(vpp,alloc);                        
  else if ((val_t) p - (val_t) large->low < large->diff)
    Enqueue(largeRoots, p);
  return alloc;
}

/* forward1_atomic_stack
   (1) Calls the underlying forward_atomic routine after
       checking that the pointer at the given location is
       in the from-space.
   (2) If the object was actually forwarded, the (newly-made) 
       forwarded object is inserted into the system thread's stack. */
static inline int forward1_atomic_stack(ploc_t vpp, mem_t *alloc, mem_t *limit, Heap_t *toheap,
					 range_t *from, SysThread_t *sysThread)
{ 
  ptr_t p = *vpp;							
  if ((val_t) p - (val_t)from->low < from->diff) {
    int bytesCopied = forward_coarse_parallel(vpp,alloc,limit,toheap);
    if (bytesCopied) {
      sysThread->LocalStack[sysThread->LocalCursor++] = (loc_t)(*vpp);
      assert(sysThread->LocalCursor < (sizeof(sysThread->LocalStack) / sizeof (ptr_t)));
      return bytesCopied;
    }
  }
  return 0;
}

/* forward2_atomic_stack
   (1) Calls the underlying forward_coarse_parallel routine after
       checking that the pointer at the given location is
       in the from-space.
   (2) If the object was actually forwarded, the (newly-made) 
       forwarded object is inserted into the system thread's stack. */
static inline int forward2_atomic_stack(ploc_t vpp, mem_t *alloc, mem_t *limit, Heap_t *toheap,
					 range_t *from, range_t *from2, range_t *large, 
					 SysThread_t *sysThread)
{ 
  ptr_t p = *vpp;							 	    
  if (((val_t) p - (val_t)from->low < from->diff) ||
      ((val_t)  p - (val_t)from2->low < from2->diff)) {
    int bytesCopied = forward_coarse_parallel(vpp,alloc,limit,toheap);
    if (bytesCopied) {
      sysThread->LocalStack[sysThread->LocalCursor++] = (loc_t)(*vpp);
      assert(sysThread->LocalCursor < (sizeof(sysThread->LocalStack) / sizeof (ptr_t)));
    }
    return bytesCopied;
  }
  else if ((val_t) p - (val_t) large->low < large->diff)
    Enqueue(sysThread->largeRoots, p);
  return 0;
}

/* forward1_concurrent_stack
   (1) Calls the underlying forward_coarse_parallel routine after
       checking that the pointer at the given location is
       in the from-space.
   (2) If the object was actually forwarded, the (newly-made) 
       forwarded object is inserted into the system thread's stack. */
static inline int forward1_concurrent_stack(ploc_t vpp, mem_t *alloc, mem_t *limit, Heap_t *toheap,
					    range_t *from, SysThread_t *sysThread)
{ 
  ptr_t p = *vpp;							
  if ((val_t) p - (val_t)from->low < from->diff) {
    int bytesCopied = forward_coarse_parallel(vpp,alloc,limit,toheap);
    if (bytesCopied) {
      sysThread->LocalStack[sysThread->LocalCursor++] = (loc_t)(*vpp);
      assert(sysThread->LocalCursor < (sizeof(sysThread->LocalStack) / sizeof (ptr_t)));
    }
    return bytesCopied;
  }
  return 0;
}

/* forward1_root_lists, forward2_root_lists
   (1) Repeatedly call forward1/forward2 on each given root.
   (2) The root_lists argument is a queue of queue of roots (ploc_t)
   (3) The upadted allocation pointer is returned.
*/
mem_t forward1_root_lists(Queue_t *root_lists, mem_t alloc,
			  range_t *from_range, range_t *to_range);
mem_t forward2_root_lists(Queue_t *root_lists, mem_t alloc,
			  range_t *from_range, range_t *from2_range, range_t *to_range,
			  range_t *large, Queue_t *largeRoots);
void forward1_writelist_atomic_stack(mem_t *alloc, mem_t *limit,
				     Heap_t *toheap, range_t *from, range_t *to, SysThread_t *sysThread);

/* forward1_writelist
   (1) Call forward1 on each location in the writelist
   (2) the writelist is terminated by a NULL 
   (3) The updated allocation pointer is returned. */
mem_t forward1_writelist(SysThread_t *sysThread, mem_t alloc,
			 range_t *from, range_t *to);



/* ------------------- Scanning Routines ---------------------- */

/* scan1_region
   (1) Scan the objects from start_scan to stop
   (2) For each object, forward_minor its pointer fields (ploc_t)
   (3) Return the new allocation pointer. */
mem_t scan1_region(mem_t start_scan, mem_t alloc, mem_t stop,
		   range_t *from_range, range_t *to_range);

/* scan1_until - standard cheney scan
   (1) Scan the objects from start_scan until there are no more objects.
       That is, keep scanning until we hit the allocation pointer.
   (2) For each object, forward_minor its pointer fields (ploc_t)
   (3) Return the new allocation pointer. */
mem_t scan1_until(mem_t start_scan, mem_t alloc,
		  range_t *from_range, range_t *to_range);

/* scan2_region
   (1) Scan the objects from start_scan to stop.
   (2) For each object, forward2 its pointer fields (ploc_t)
   (3) Return the new allocation pointer. */
mem_t scan2_region(mem_t start_scan, mem_t alloc, mem_t stop, 
		   range_t *from_range, range_t *from2_range, range_t *to_range,
		   range_t *large, Queue_t *largeRoots);

/* scan1_object_atomic_stack
   (1) Scan the given object
   (2) For each pointer field, call scan_object_minor_atomic_stack
   (3) The allocation/limit pointers and stack may be updated. */
int scan1_object_atomic_stack(ptr_t gray_obj, mem_t *alloc_ptr, mem_t *limit_ptr, Heap_t *toheap,
			       range_t *from_range, range_t *to_range, SysThread_t *sysThread);
int scan2_object_atomic_stack(ptr_t gray_obj, mem_t *alloc_ptr, mem_t *limit_ptr, Heap_t *toheap,
			       range_t *from_range, range_t *from2_range,
			       range_t *to_range, SysThread_t *sysThread);
int scan1_object_conc_stack(ptr_t gray_obj, mem_t *alloc_ptr, mem_t *limit_ptr, Heap_t *toheap,
			     range_t *from_range, range_t *to_range, SysThread_t *sysThread);

#endif
