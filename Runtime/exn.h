/* exn_init should be called once; raise_exception is thread-safe */

#ifndef _exn_h
#define _exn_h

#include "thread.h"
#include "tag.h"

void exn_init(void);

ptr_t exnNameRuntime(ptr_t);
ptr_t exnMessageRuntime(ptr_t);

void raise_exception(ucontext_t *, ptr_t);
void raise_exn(ptr_t);

void toplevel_exnhandler(Thread_t *);
	
/* From Basis - access is safe only though GetGlobal */
ptr_t getOverflowExn(void);
ptr_t getDivExn(void);
ptr_t mkSysErrExn(ptr_t, int, int);

#endif
