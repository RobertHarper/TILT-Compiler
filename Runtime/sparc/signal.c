#include "s.h"
#include "r.h"
#include "sparc.h"
#include <signal.h>

/*
#include <sys/sysinfo.h>
#include <sys/proc.h>
#ifdef sparc
#include <siginfo.h>
#endif
*/

extern int	my_sigaction(int, const struct sigaction*, struct sigaction*);

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

void memfault_handler(int signum, 
		      siginfo_t *siginfo, 
		      ucontext_t *uctxt)
{
  Proc_t *proc = getProc();
  int code = siginfo->si_code;
  mem_t badaddr = GetBadAddr(uctxt,siginfo);
  mem_t badpc = GetPc(uctxt);

  assert(signum == (siginfo->si_signo));

  printf("Proc %d:  Memory error at 0x%lx with PC = 0x%lx\n",proc->procid, (long)badaddr, (long)badpc);
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
	  printf("SEGV_MAPERR  address %lx not mapped\n",(long)badaddr);
	  break;
	case SEGV_ACCERR:
	  printf("SEGV_ACCERR  invalid permissions for address %lx\n",
		 (long)badaddr);
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
	    printf("BUS_ADRALN invalid address alignment\n");
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
  int e = siginfo->si_errno;
  int code = siginfo->si_code;
  if (signum != SIGFPE || signo != SIGFPE)
    printf("BUG: fpe_handler for non fpe signal: signum = %d, signo = %d\n",signum,signo);
  switch (code)
    {
    case FPE_INTDIV:
      if (paranoid) 
	printf("Integer divide by zero: %d %d ",e,code);
      unblock_signals();
      raise_exception(uctxt,getDivExn());
      break;
    case FPE_FLTDIV:
      if (paranoid) 
	printf("Float divide by zero: %d %d ",e,code);
      unblock_signals();
      raise_exception(uctxt,getDivExn());
      break;
    case FPE_INTOVF:
      if (paranoid) 
	printf("Integer overflow: we are not getting this... %d %d ",e,code);
      unblock_signals();
      raise_exception(uctxt,getOverflowExn());
      break;
    case FPE_FLTOVF: 
      if (paranoid)
	printf("Float OR integer overflow: %d %d ",e,code);
      unblock_signals();
      raise_exception(uctxt,getOverflowExn());
      break;
    case FPE_FLTUND:
      printf("Float underflow: %d %d ",e,code);
      break;
    case FPE_FLTRES:
      printf("Float inexact result: %d %d ",e,code);
      break;
    case FPE_FLTINV:
      /* We need to fix the result register but can't seem to use the default handler */
      printf("Invalid Float operation; probably NAN operand; cannot patch result, resuming anyway\n");
      return;

    case FPE_FLTSUB:
      printf("Float subscript out of range: %d %d ",e,code);
      break;
    default:
      printf("Unknown FPE signal: %d %d ",e,code);
      break;
    }
  exit(-1);
}

void signal_init(void)
{
}

void install_signal_handlers(int isMain)
{
  typedef void (*voidhandler_t)();
  struct sigaction newact;

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
