/* Assumes exceptions look a certain way in the creation of the divide and overflow exception */
#include "general.h"
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
/*
#include <siginfo.h>
#include <machine/fpu.h>
*/
#include <sys/sysinfo.h>
#include <sys/proc.h>
#include <string.h>
#include "tag.h"
#include "create.h"
#include "til-signal.h"
#include "global.h"
#include "thread.h"
#include "exn.h"

ptr_t divide_exn;
ptr_t overflow_exn;
extern ptr_t Divide_exncon;
extern ptr_t Overflow_exncon;
extern void raise_exception_raw(Thread_t *th, ptr_t exn_arg);


void exn_init()
{
  static int buf[100];
  static mem_t alloc=0, limit=0;
  if (!alloc) {
    alloc = (mem_t) buf;
    limit = alloc + 100 * sizeof(int);
  }
  divide_exn = alloc_recrec((ptr_t) *Divide_exncon,0);
  overflow_exn = alloc_recrec((ptr_t) *Overflow_exncon,0);
}

void raise_exception(struct ucontext *uctxt, ptr_t exn_arg)
{
  Thread_t *th = getThread();
  ptr_t exn_ptr;
  mem_t code;

  /* Move saved register from uctxt to thread area so it can be restored by raise_exception_raw */
  GetIRegs(uctxt, th->saveregs);

#ifdef DEBUG
  {
    int i;
    exn_ptr = th->saveregs[EXNPTR];
    code = get_record(exn_ptr,0);

    fprintf(stderr,"\n\n--------exn_raise entered---------\n");
    fprintf(stderr,"raise: exn_ptr is %d\n",exn_ptr);
    fprintf(stderr,"raise: rec[-1] is %d\n",((int *)exn_ptr)[-1]);
    fprintf(stderr,"raise: rec[0] is %d\n",((int *)exn_ptr)[0]);
    for (i=0; i<32; i++)
      fprintf(stderr,"RAISE: the_iregs[%d] is %ld\n",i,iregs[i]);
    fprintf(stderr,"returning from exn_raise to asm linkage\n");
  }
#endif
  
  raise_exception_raw(th,exn_arg);
}

void toplevel_exnhandler(Thread_t *th)
{
  char buf[100];
  char *msg;
  long *saveregs = th->saveregs;
  ptr_t exn_arg = (ptr_t)saveregs[EXNARG];
  val_t first = get_record(exn_arg,0);

  if (first == *Divide_exncon)
    { msg = "Divide by zero"; }
  else if (first == *Overflow_exncon)
    { msg = "Overflow"; }
  else
    {
      ptr_t name = (ptr_t) get_record(exn_arg,2);
      unsigned int tag = ((int *)name)[-1];
      int bytelen = GET_ARRLEN(tag);
      bcopy((char *)name,buf,bytelen);
      buf[bytelen] = 0;
      msg = buf;
    }
  
  printf("Proc %d %d, Thread %d (%d): Uncaught exception: %s\n",
	 getSysThread()->processor,
	 th->sysThread->processor,th->tid,th->id,msg);
  Finish();
}
