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

extern void raise_exception_raw(Thread_t *th, ptr_t exn_arg);

/* Unlike Div and Overflow, we don't have a canonical exception
 * packet for these exceptions.  The search for the exception stamp
 * has to happen relatively late (global_init isn't early enough).
 */
extern ptr_t RuntimeError_r_INT, RuntimeErrorPRIME_r_INT;

static int getExnStamp(ptr_t exnstructure)
{
   int stamp = *((int*) exnstructure); /* first field: 32-bit integer stamp */
   return stamp;
}

int RuntimeStamp(void)
{
  return getExnStamp(RuntimeError_r_INT);
}

int RuntimePrimeStamp(void)
{
  return getExnStamp(RuntimeErrorPRIME_r_INT);
}

void exn_init()
{
  static int buf[100];
  static mem_t alloc=0, limit=0;
  if (!alloc) {
    alloc = (mem_t) buf;
    limit = alloc + 100 * sizeof(int);
  }
}

void raise_exception(struct ucontext *uctxt, ptr_t exn_arg)
{
  Thread_t *th = getThread();
  mem_t code;

  /* Move saved register from uctxt to thread area so it can be restored by raise_exception_raw */
  GetIRegs(uctxt, th->saveregs);

#ifdef DEBUG
  {
    int i;
    ptr_t exn_ptr = th->saveregs[EXNPTR];
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

void raise_exn(ptr_t exnname, int exnstamp, val_t exnarg, int argPointer)
{
  val_t fields[3];
  int mask = argPointer ? 3 : 1;
  ptr_t exn;
  Thread_t* th = getThread();
  fields[0] = exnstamp;
  fields[1] = exnarg;
  fields[2] = (val_t)exnname;
  exn = alloc_record(fields, &mask, 3);
  raise_exception_raw(th, exn);
}

void printString(ptr_t);
ptr_t exnMessageRuntime(ptr_t);

void toplevel_exnhandler(Thread_t *th)
{
  char* msg = "";
  unsigned long *saveregs = th->saveregs;
  ptr_t exn = (ptr_t)saveregs[EXNARG];

  printf("Proc %d: Thread %d (%d): Uncaught exception ",
	 getProc()->stid, th->tid, th->id);
  printString(exnMessageRuntime(exn));
  printf("\n");
  Finish();
}
