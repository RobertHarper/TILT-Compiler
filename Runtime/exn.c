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
#include "client.h"

extern void raise_exception_raw(Thread_t *th, ptr_t exn_arg);
extern int stringlen(ptr_t string);
extern ptr_t cstring2mlstring_alloc(const char *);

#define GET_RECORD(t, p, i) ((t)get_record((p),(i)))
#define GET_PTR(p, i) GET_RECORD(ptr_t,p,i)
#define GET_INT(p, i) GET_RECORD(int,p,i)

/* The declarations in ../Basis/tiltexn.sml. */
#define DIV 0
#define OVERFLOW 1
#define TILTEXN 2

/* The components of an exception module.
   N.B. must agree with ../Elaborator/toil.sml. */
#define MOD_STAMP 0
#define MOD_MK    1

/* Find canonical exception packet for a top-level, non-value-carrying
   exception declared in unit TiltExn; see ../Basis/tiltexn.sml. */
static ptr_t exn_lookup(int n)
{
  ptr_t compunit = (ptr_t) GetGlobal(&ml__PLUSUTiltExn__INT__r__INT);
  ptr_t exnmod = (ptr_t) GET_PTR(compunit,n);
  return GET_PTR(exnmod, MOD_MK);
}

ptr_t getOverflowExn(void)
{
  return exn_lookup(OVERFLOW);
}

ptr_t getDivExn(void)
{
  return exn_lookup(DIV);
}

/* The components of an exception packet. */
#define PACKET_STAMP 0
#define PACKET_ARG   1
#define PACKET_NAME  2

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

/* The components of the TiltExn structure; see ../Basis/tiltexn.sml. */
#define SYSERR  0
#define LIBFAIL 1

/* Get the exception stamp for the ith TiltExn componenet. */
static int getTiltExnStamp(int i)
{
  ptr_t compunit = (ptr_t) GetGlobal(&ml__PLUSUTiltExn__INT__r__INT);
  ptr_t tiltexn = GET_PTR(compunit,TILTEXN);
  ptr_t exnmod = GET_PTR(tiltexn, i);
  int stamp = GET_INT(exnmod, MOD_STAMP);
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

  if (th == NULL)
    /* This happens, for example, if the runtime
       divides by zero during initialization.  */
    DIE("spurious exception");

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

  fprintf(stderr,"Proc %d: Thread %d (%d): Uncaught exception %.*s\n",
	  getProc()->procid, th->tid, th->id, msg_len, (char*)msg);
  Finish();
}

void RestoreStackFromMutator(Thread_t* th)
{
  mem_t sp = (mem_t) th->saveregs[SP];
  mem_t ra = (mem_t) th->saveregs[RA];
  Stacklet_t* oldStacklet = CurrentStacklet(th->stack);
  Stacklet_t* newStacklet = EstablishStacklet(th->stack, sp);
  assert(oldStacklet != newStacklet);
  th->stackLimit = StackletPrimaryBottom(newStacklet);
  th->stackTop = StackletPrimaryTop(newStacklet);
  Stacklet_KillReplica(newStacklet);
  returnToML(th, ra);
  abort();
}
