#include "general.h"
#include <stdlib.h>
#include <stdio.h>

#include <sys/sysinfo.h>
#include <sys/proc.h>

#include "memobj.h"
#include "thread.h"
#include "exn.h"
#include "til-signal.h"
#include "thread.h"
#include "global.h"

#include <signal.h>

#ifdef alpha
#include <siginfo.h>
#include <machine/fpu.h>
#endif
#ifdef sparc
#include <siginfo.h>
#endif

extern int my_sigaction(int signal, const struct sigaction* action, struct sigaction* o_action);

#define WRITE

#ifdef alpha
mem_t GetPc(ucontext_t *uctxt)          { return (mem_t) (uctxt->uc_mcontext.sc_pc); }
mem_t GetSp(ucontext_t *uctxt)          { return (mem_t) (uctxt->uc_mcontext.sc_sp); }
unsigned long GetIReg(ucontext_t *uctxt, int i) { return (uctxt->uc_mcontext.sc_regs[i]); }
void SetIReg(ucontext_t *uctxt, int i, unsigned long v) { uctxt->uc_mcontext.sc_regs[i] = v; }
double GetFReg(ucontext_t *uctxt, int i) { return (uctxt->uc_mcontext.sc_fpregs[i]); }
void SetFReg(ucontext_t *uctxt, int i, double v) { uctxt->uc_mcontext.sc_fpregs[i]= v; }
mem_t GetBadAddr(ucontext_t *uctxt, 
		 siginfo_t *siginfo)   { return (mem_t) (siginfo->si_addr); }
#endif


#ifdef sparc
unsigned long GetIReg(ucontext_t *uctxt, int i)    
{ 
  if (i == 0)
    return 0;
  if (i <= 15)
    return uctxt->uc_mcontext.gregs[REG_G1 + (i - 1)];
  if (i < 32) {
    gwindows_t *gwins = uctxt->uc_mcontext.gwins;
    if (gwins != NULL) {
      /* window saved here */
      if (i < 24)
	return (gwins->wbuf[gwins->wbcnt].rw_local[i-16]);
      else 
	return (gwins->wbuf[gwins->wbcnt].rw_in[i-24]);
    }
    else { 
      /* window saved on stack */
      int *sp = (int *)(uctxt->uc_mcontext.gregs[REG_SP]);
      return sp[i - 16];
    }
  }
  DIE("bad register number in GetIReg");
}

void SetIReg(ucontext_t *uctxt, int i, unsigned long v)    
{ 
  if (i == 0)
    return;
  if (i <= 15)
    uctxt->uc_mcontext.gregs[REG_G1 + (i - 1)] = v;
  if (i < 32) {
    gwindows_t *gwins = uctxt->uc_mcontext.gwins;
    if (gwins != NULL) {
      if (i < 24)
	(gwins->wbuf[gwins->wbcnt].rw_local[i-16]) = v;
      else 
	(gwins->wbuf[gwins->wbcnt].rw_in[i-24]) = v;
    } else
      DIE("bad ucontext in SetIReg");
  }
}
mem_t GetPc(ucontext_t *uctxt)    { return (mem_t) (uctxt->uc_mcontext.gregs[REG_PC]); }
mem_t GetSp(ucontext_t *uctxt)    { return (mem_t) (GetIReg(uctxt,SP)); }
mem_t GetBadAddr(ucontext_t *uctxt, 
		 siginfo_t *siginfo)   { return (mem_t) (siginfo->si_addr); }
#endif

void GetIRegs(ucontext_t *uctxt, unsigned long *dest) 
{ 
  int i;
  for (i=0; i<32; i++)
    dest[i] = GetIReg(uctxt,i);
}

#ifdef alpha
void float_exn_on(void)
{
  /*
  ieee_set_fp_control( IEEE_TRAP_ENABLE_INV        
     
		      IEEE_TRAP_ENABLE_DZE        
		      IEEE_TRAP_ENABLE_OVF
		       );
*/
}
#endif
#ifdef sparc
void float_exn_on(void)
{
  printf("need to implemented float_exn_on for SPARC");
}
#endif

#ifdef alpha
void buserror_on(void)
{
  int buf[2], error;
  
  /* Deliver a SIGBUS signal and don't print the warning */
  buf[0] = SSIN_UACPROC;
  buf[1] = UAC_SIGBUS | UAC_NOPRINT;
  error = setsysinfo(SSI_NVPAIRS,buf, 1, 0, 0);
  assert(!error);
}
#else
void buserror_on(void)
{}
#endif

#ifndef NSIG
#define NSIG __sys_nsig
#endif

static void unblock_signals(void)
{
  sigset_t oset;
  sigset_t set;
  if (sigfillset(&set) == -1 ||
      sigprocmask(SIG_UNBLOCK, &set, &oset) == -1) {
    perror("can't clear signal mask");
    exit(-1);
  }
}

/*
void signaltest(void)
{
  static int which = 3;
  static int dummy = 567;
  printf("signatest(%d): ",which);
  switch (which++)
    {
    case 0:
      dummy /= zero();
      break;
    case 1:
      dummy += 45.0 / ((double)zero());
      break;
    case 2:
      {
	double foo = 3e150;
	foo *= foo * foo;
	dummy = foo;
      }
      break;
    case 3:
      {
	double foo = 3456e-150;
	foo *= foo * foo;
	dummy = foo;
      }
      break;
    case 4:
      *((int *)8) = dummy;
      break;
    case 5:
      {
	int x = (int)(&dummy);
	*((int *)(x+1)) = dummy;
      }
      break;
    case 6:
      {
	typedef void (*F)();
	int i,badcode[20];
	F foo = (F)badcode;
	for (i=0; i<20; i++)
	  badcode[0] = 0;
	foo();
      }
      break;
    case 9:
      dummy = (1+zero()) << 30;
      dummy += dummy;
      dummy += dummy;
      printf("%ld\n",dummy);
      break;
    }
  printf("done\n");
  exit(-1);
}
*/

void memfault_handler(int signum, 
		      siginfo_t *siginfo, 
		      ucontext_t *uctxt)
{
  Proc_t *proc = getProc();
  int code = siginfo->si_code;
  mem_t badaddr = GetBadAddr(uctxt,siginfo);
  mem_t badpc = GetPc(uctxt);

  assert(signum == (siginfo->si_signo));

  printf("Proc %d:  Memory error at 0x%x with PC = 0x%x\n",proc->procid, badaddr, badpc);
  switch (signum)
    {
    case SIGILL:
      {
	printf("Illegal instruction\n");
	break;
      }
    case SIGSEGV:
      {
      switch (code)
	{ 
	case SEGV_MAPERR:
	  printf("SEGV_MAPERR  address %d not mapped\n",badaddr);
	  break;
	case SEGV_ACCERR:
	  printf("SEGV_ACCERR  invalid permissions for address %d\n",
		 proc->procid,badaddr);
	  if (StackError(uctxt,badaddr))
	    printf("Stackrelink/overflow not implemented\n");
	  break;
	default:
	  printf("UNKNOWN      ");
	  break;
	}
      break;
      }
    case SIGBUS:  /* BUS ERROR */
      {
	printf("Bus Error\n");
	switch (code)
	  {
	  case BUS_ADRALN:
	    printf("BUS_ADRALN invalid address alignment\n",proc->procid);
	    break;
	  case BUS_ADRERR:
	    printf("BUS_ADRERR non-existent physical address\n");
	    break;
	  case BUS_OBJERR:
	    printf("BUS_OBJERR object specific hardware error\n");
	  default:
	    printf("UNKNOWN\n");
	  break;
	  }
	break;
      }
    }


  {
    Heap_t *heap = GetHeap(badaddr);
    Stacklet_t *stacklet = GetStacklet(badaddr);
    if (heap != NULL)
      printf("   addr part of a heap obj\n");
    if (stacklet != NULL)
      printf("   addr part of a stacklet obj\n");
  }
  exit(-1);
}

typedef void (*sa_sigaction_t)(int, siginfo_t*, void*);
struct sigaction old_fpe_action;
void fpe_handler(int signum, 
		 siginfo_t *siginfo, 
		 ucontext_t *uctxt)
{

  int signo = siginfo->si_signo;
  int errno = siginfo->si_errno;
  int code = siginfo->si_code;
  if (signum != SIGFPE || signo != SIGFPE)
    printf("BUG: fpe_handler for non fpe signal: signum = %d, signo = %d\n",signum,signo);
  switch (code)
    {
    case FPE_INTDIV:
      if (paranoid) 
	printf("Integer divide by zero: %d %d ",errno,code);
      unblock_signals();
      raise_exception(uctxt,getDivExn());
      break;
    case FPE_FLTDIV:
      if (paranoid) 
	printf("Float divide by zero: %d %d ",errno,code);
      unblock_signals();
      raise_exception(uctxt,getDivExn());
      break;
    case FPE_INTOVF:
      if (paranoid) 
	printf("Integer overflow: we are not getting this... %d %d ",errno,code);
      unblock_signals();
      raise_exception(uctxt,getOverflowExn());
      break;
    case FPE_FLTOVF: 
      if (paranoid)
	printf("Float OR integer overflow: %d %d ",errno,code);
      unblock_signals();
      raise_exception(uctxt,getOverflowExn());
      break;
    case FPE_FLTUND:
      printf("Float underflow: %d %d ",errno,code);
      break;
    case FPE_FLTRES:
      printf("Float inexact result: %d %d ",errno,code);
      break;
    case FPE_FLTINV:
      /* We need to fix the result register but can't seem to use the default handler */
      printf("Invalid Float operation; probably NAN operand; cannot patch result, resuming anyway\n");
      return;

    case FPE_FLTSUB:
      printf("Float subscript out of range: %d %d ",errno,code);
      break;
    default:
      printf("Unknown FPE signal: %d %d ",errno,code);
      break;
    }
  exit(-1);
}

void signal_init(void)
{
  buserror_on();
  float_exn_on();
}

void install_signal_handlers(int isMain)
{
  typedef void (*voidhandler_t)();
  struct sigaction newact, oldact;

  sigfillset(&newact.sa_mask);
  newact.sa_flags = SA_SIGINFO /* | SA_NODEFER -- conflicts with sa_mask */;

  if (!isMain) {
    newact.sa_sigaction = (sa_sigaction_t) fpe_handler;
    my_sigaction(SIGFPE,&newact,&old_fpe_action); 
    newact.sa_sigaction = (sa_sigaction_t) memfault_handler;
    my_sigaction(SIGSEGV,&newact,NULL);
    my_sigaction(SIGBUS,&newact,NULL); 
    my_sigaction(SIGILL,&newact,NULL);
  }
  { /* Main thread is asleep so we don't want it to handle signals */
    sigset_t sigset;
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGINT);
    sigaddset(&sigset, SIGILL);
    sigaddset(&sigset, SIGSEGV);
    sigaddset(&sigset, SIGBUS);
    pthread_sigmask(isMain ? SIG_BLOCK : SIG_UNBLOCK,  &sigset, NULL);
  }
}
