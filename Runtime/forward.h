#ifndef _forward_h
#define _forward_h

#include "memobj.h"
#include "thread.h"

struct range_st
{
  value_t low;
  value_t high;
  value_t diff;
};

typedef struct range_st range_t;

void SetRange(range_t *range, value_t low, value_t high);


/* Forwarding:
   (1) Takes the address of a pointer to an object (of any color)
   (2) Copies the object, if necessary, to the tospace
   (3) Updates the address to the new pointer
   (4) Returns the new pointer
   Different routines take one or two fromspaces.
   Scanning:
   (1) Takes the pointer to an object (presumed gray)
   (2) Decodes the object to find all pointer fields
   (3) Forwards each of the fields (thus updating the fields and making the object black)
   (4) Possibly put the new gray forwarded objects onto a work queue
*/


/* --------------- Forwarding Routines ---------------------------- */
value_t *forward(value_t *vpp, value_t *alloc);  /* Don't call directly */
void forward_stack(value_t *vpp, value_t **alloc, value_t **limit, Heap_t *toheap);  /* Don't call directly */

#define forward_minor(vpp,alloc,from)  \
    { if (((*(vpp)) - (from)->low) < (from)->diff) alloc = forward(vpp,alloc); }
#define forward_major(vpp,alloc,from,from2,to)  \
    { if ((((*(vpp)) - (from)->low) < (from)->diff) ||  \
	  (((*(vpp)) - (from2)->low) < (from2)->diff)) alloc = forward(vpp,alloc); }
#define forward_local_minor(vpp,alloc)  \
    { if (((*(vpp)) - local_from_low) < local_from_diff) alloc = forward(vpp,alloc); }

#define forward_stack_help(vpp,alloc,limit,toheap,from,sysThread)  \
    { forward_stack(vpp,&alloc,&limit,toheap); \
	sysThread->LocalStack[sysThread->LocalCursor++] = (*(vpp)); \
    } 

#define forward_minor_stack(vpp,alloc,limit,toheap,from,sysThread)  \
{ if (((*(vpp)) - (from)->low) < (from)->diff) forward_stack_help(vpp,alloc,limit,toheap,from,sysThread); }
#define forward_local_minor_stack(vpp,alloc,limit,toheap,sysThread)  \
{ if (((*(vpp)) - local_from_low) < local_from_diff) forward_stack_help(vpp,alloc,limit,toheap,from,sysThread); }

value_t *forward_root_lists_minor(Queue_t *root_lists, value_t *to_ptr, 
				  range_t *from_range, range_t *to_range);
value_t *forward_root_lists_major(Queue_t *root_lists, value_t *to_ptr, 
				  range_t *from_range, range_t *from2_range, range_t *to_range);

/* ------------------- Scanning Routines ---------------------- */
value_t * scan_oneobject_major(value_t **where,  value_t *alloc,
			       range_t *from_range, range_t *from2_range, range_t *to_range);
value_t* scan_major(value_t start_scan, value_t *alloc, value_t *stop, 
		    range_t *from_range, range_t *from2_range, range_t *to_range);
value_t* scan_nostop_minor(value_t start_scan, value_t *alloc,
			   range_t *from_range, range_t *to_range);
value_t* scan_stop_minor(value_t start_scan, value_t *alloc, value_t *stop,
			 range_t *from_range, range_t *to_range);

void scan_minor_stack(value_t *gray_obj, value_t **alloc_ptr, value_t **limit_ptr, Heap_t *toheap,
		      range_t *from_range, range_t *to_range, SysThread_t *sysThread);

void scan_oneobject_for_pointers(value_t *gray, Queue_t *queue);

#endif
