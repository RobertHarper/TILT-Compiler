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
#include "stack.h"
#include "exn.h"

extern void raise_exception_raw(Thread_t *th, ptr_t exn_arg);
extern int stringlen(ptr_t string);
extern ptr_t cstring2mlstring_alloc(const char *);

extern val_t Div_r_INT, Overflow_r_INT;   /* Must use & to get global's address */
extern ptr_t TiltExn_STR_r_INT;

ptr_t getOverflowExn(void)
{
  return (ptr_t) GetGlobal(&Overflow_r_INT);
}

ptr_t getDivExn(void)
{
  return (ptr_t) GetGlobal(&Div_r_INT);
}

#define PACKET_STAMP 0		/* The components of an exception */
#define PACKET_ARG 1		/* packet. */
#define PACKET_NAME 2

static ptr_t mkExn(ptr_t exnname, int exnstamp, val_t exnarg, int argPointer)
{
  val_t fields[3];
  int mask = argPointer ? 6 : 4;
  ptr_t exn;
  fields[PACKET_STAMP] = exnstamp;
  fields[PACKET_ARG]   = exnarg;
  fields[PACKET_NAME]  = (val_t)exnname;
  exn = alloc_record(fields, &mask, 3);
  return exn;
}

/* Unlike Div and Overflow, we don't have a canonical exception
 * packet for SysErr (which carries a value).  The search for
 * the exception stamp has to happen relatively late (global_init
 * isn't early enough).
 */

#define GET_RECORD(t, p, i) ((t)get_record((p),(i)))
#define GET_PTR(p, i) GET_RECORD(ptr_t,p,i)
#define GET_INT(p, i) GET_RECORD(int,p,i)

#define DEC_STAMP 0		/* the components of the structure corresponding to an exception */
#define DEC_INJECT 1		/* declaration -- must agree with Elaborator/toil.sml */

#define SYSERR 0		/* the components of the TiltExn structure -- must agree with */
#define LIBFAIL 1		/* Basis/Firstlude.sml */

static int getTiltExnStamp(int i)
{
  ptr_t str = GET_PTR(TiltExn_STR_r_INT, i);
  int stamp = GET_INT(str, DEC_STAMP);
  return stamp;
}
  
ptr_t mkSysErrExn(ptr_t msg, int isSome, int errno)
{
  ptr_t exnname = cstring2mlstring_alloc("SysErr");
  int exnstamp = getTiltExnStamp(SYSERR);
  ptr_t errno_option = isSome ? alloc_manyint(1, errno) : 0;
  ptr_t exnarg = alloc_recrec(msg, errno_option);
  ptr_t exn = mkExn(exnname, exnstamp, (val_t)exnarg, 1);
  return exn;
}

#define GET_STAMP(exn) GET_INT((exn), PACKET_STAMP)
#define GET_ARG(exn)   GET_PTR((exn), PACKET_ARG)
#define GET_NAME(exn)  GET_PTR((exn), PACKET_NAME)

ptr_t exnNameRuntime(ptr_t exn)
{
  return GET_NAME(exn);
}

ptr_t exnMessageRuntime(ptr_t exn)
{
  char buf[1024];
  int exnstamp = GET_STAMP(exn);
  
  if (exnstamp == GET_STAMP(getDivExn())) {
    strcpy(buf, "divide by zero");
  } else if (exnstamp == GET_STAMP(getOverflowExn())) {
    strcpy(buf, "overflow");
  } else if (exnstamp == getTiltExnStamp(LIBFAIL)) {
    const char* prefix = "LibFail: ";
    ptr_t msg = GET_ARG(exn);
    int msg_len = stringlen(msg);
    assert(sizeof(buf) > sizeof(prefix) + msg_len);
    sprintf(buf, "%s%.*s", prefix, msg_len, msg);
  } else if (exnstamp == getTiltExnStamp(SYSERR)) {
    ptr_t exnarg = GET_ARG(exn);
    const char* prefix = "SysErr: ";
    ptr_t msg = GET_PTR(exnarg, 0);
    int msg_len = stringlen(msg);
    ptr_t err_option = GET_PTR(exnarg, 1);
    int isSome = err_option != 0;
    int err = isSome ? GET_INT(err_option, 0) : 0;
    char err_buf[40];
    if (isSome) {
      sprintf(err_buf, " (errno=%d)", err);
    } else {
      *err_buf = '\0';
    }
    assert(sizeof(buf) > sizeof(prefix) + msg_len + strlen(err_buf));
    sprintf(buf, "%s%.*s%s", prefix, msg_len, (char*)msg, err_buf);
  } else {
    ptr_t name = GET_NAME(exn);
    int name_len = stringlen(name);
    assert(sizeof(buf) > name_len);
    sprintf(buf, "%.*s", name_len, (char*)name);
  }
  return cstring2mlstring_alloc(buf);
}

void exn_init(void)
{
  static int buf[100];
  static mem_t alloc=0, limit=0;
  if (!alloc) {
    alloc = (mem_t) buf;
    limit = alloc + 100 * sizeof(int);
  }
}

void raise_exn(ptr_t exn)
{
  Thread_t* th = getThread();
  raise_exception_raw(th, exn);
}

void raise_exception(ucontext_t *uctxt, ptr_t exn_arg)
{
  Thread_t *th = getThread();

  if (!th->notInML) {
    /* Move saved register from uctxt to thread area so it can be restored by raise_exception_raw */
    GetIRegs(uctxt, (unsigned long *) th->saveregs);
  }

  if (debug) {
    int i;
    ptr_t exn_ptr = (ptr_t) th->saveregs[EXNPTR];
    val_t code = get_record(exn_ptr,0);

    fprintf(stderr,"\n\n--------exn_raise entered---------\n");
    fprintf(stderr,"raise: exn_ptr is %d\n",exn_ptr);
    fprintf(stderr,"raise: rec[-1] is %d\n",((int *)exn_ptr)[-1]);
    fprintf(stderr,"raise: rec[0] is %d\n",((int *)exn_ptr)[0]);
    for (i=0; i<32; i++)
      fprintf(stderr,"RAISE: the_iregs[%d] is %ld\n",i,th->saveregs[i]);
    fprintf(stderr,"returning from exn_raise to asm linkage\n");
  }
  
  raise_exception_raw(th,exn_arg);
}

void toplevel_exnhandler(Thread_t *th)
{
  volatile unsigned long *saveregs = th->saveregs;
  ptr_t exn = (ptr_t)saveregs[EXNARG];
  ptr_t msg = exnMessageRuntime(exn);
  int msg_len = stringlen(msg);

  printf("Proc %d: Thread %d (%d): Uncaught exception %.*s\n",
	 getProc()->procid, th->tid, th->id, msg_len, (char*)msg);
  Finish();
}
