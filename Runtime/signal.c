#include "general.h"
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <sys/sysinfo.h>
#include <sys/proc.h>

#include "memobj.h"
#include "thread.h"
#include "exn.h"
#include "til-signal.h"
#include "thread.h"
#include "global.h"

#ifdef alpha_osf
#include <siginfo.h>
#include <machine/fpu.h>
#endif
#ifdef solaris
#include <siginfo.h>
#endif
#ifdef rs_aix
#include <fptrap.h> 
#endif

#define WRITE


#ifdef alpha_osf
mem_t GetPc(struct ucontext *uctxt)          { return (mem_t) (uctxt->uc_mcontext.sc_pc); }
mem_t GetSp(struct ucontext *uctxt)          { return (mem_t) (uctxt->uc_mcontext.sc_sp); }
unsigned long GetIReg(struct ucontext *uctxt, int i) { return (uctxt->uc_mcontext.sc_regs[i]); }
void SetIReg(struct ucontext *uctxt, int i, unsigned long v) { uctxt->uc_mcontext.sc_regs[i] = v; }
double GetFReg(struct ucontext *uctxt, int i) { return (uctxt->uc_mcontext.sc_fpregs[i]); }
void SetFReg(struct ucontext *uctxt, int i, double v) { uctxt->uc_mcontext.sc_fpregs[i]= v; }
mem_t GetBadAddr(struct ucontext *uctxt, 
		 siginfo_t *siginfo)   { return (mem_t) (siginfo->si_addr); }
#endif


#ifdef rs_aix
scp is sigcontext obtained from uctxt
mem_t GetPc(struct ucontext *uctxt)    { return (mem_t) ((scp)->sc_jmpbuf.jmp_context.iar); }
mem_t GetSp(struct ucontext *uctxt)    { return (mem_t) ((scp)->sc_jmpbuf.jmp_context.gpr[1]); }
unsigned long *GetIRegs(struct ucontext *uctxt) { return &((scp)->sc_jmpbuf.jmp_context.gpr[0]); }
mem_t GetBadAddr(struct ucontext *uctxt, int dummy) { return (mem_t)((scp)->sc_jmpbuf.jmp_context.o_vaddr); }
#endif

#ifdef solaris
unsigned long GetIReg(struct ucontext *uctxt, int i)    
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
  assert(0);
}

void SetIReg(struct ucontext *uctxt, int i, unsigned long v)    
{ 
  if (i == 0)
    return;
  if (i <= 15)
    uctxt->uc_mcontext.gregs[REG_G1 + (i - 1)] = v;
  if (i < 32) {
    gwindows_t *gwins = uctxt->uc_mcontext.gwins;
    if (gwins != NULL)
      if (i < 24)
	(gwins->wbuf[gwins->wbcnt].rw_local[i-16]) = v;
      else 
	(gwins->wbuf[gwins->wbcnt].rw_in[i-24]) = v;
    else assert(0);
  }
}
mem_t GetPc(struct ucontext *uctxt)    { return (mem_t) (uctxt->uc_mcontext.gregs[REG_PC]); }
mem_t GetSp(struct ucontext *uctxt)    { return (mem_t) (GetIReg(uctxt,SP)); }
mem_t GetBadAddr(struct ucontext *uctxt, 
		 siginfo_t *siginfo)   { return (mem_t) (siginfo->si_addr); }
#endif

void GetIRegs(struct ucontext *uctxt, unsigned long *dest) 
{ 
  int i;
  for (i=0; i<32; i++)
    dest[i] = GetIReg(uctxt,i);
}

int zero()
{
  return 0;
}

#ifdef alpha_osf
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
#ifdef rs_aix
void float_exn_on(void)
{
  fp_enable_all();
}
#endif
#ifdef solaris
void float_exn_on(void)
{
  printf("need to implemented float_exn_on for SPARC");
}
#endif

#ifdef alpha_osf
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


/*
void signaltest()
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
#if (defined alpha_osf) || (defined solaris)
		      siginfo_t *siginfo, 
#elif (defined rs_aix)
		      int always_zero,
#endif
		      struct ucontext *uctxt)
{
  Proc_t *proc = getProc();
#if (defined alpha_osf) || (defined solaris)
  int code = siginfo->si_code;
#elif (defined rs_aix)
  siginfo_t *siginfo = NULL;
  int code = 0;
#endif
  mem_t badaddr = GetBadAddr(uctxt,siginfo);
  mem_t badpc = GetPc(uctxt);

#if (defined alpha_osf) || (defined solaris)
  assert(signum == (siginfo->si_signo));
#endif

  printf("Proc %d:  Memory error at %d with PC = %d\n",proc->procid, badaddr, badpc);
  switch (signum)
    {
    case SIGILL:
      {
	printf("Illegal instruction\n");
	exit(-1);
      }
    case SIGSEGV:
      {
#if (defined alpha_osf) || (defined solaris)
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
#elif (defined rs_aix)
      if (StackError(uctxt,badaddr))
	printf("Stackrelink/overflow not implemented\n");
#else
      printf("AIX NOT DONE\n");
#endif
      break;
      }
    case SIGBUS:  /* BUS ERROR */
      {
	printf("Bus Error\n");
#if (defined alpha_osf || defined solaris)
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
#elif (defined rs_aix)
	printf("AIX NOT DONE\n");
#else
	printf("UNKNOWN PLATFORM\n");
#endif
	break;
      }
    }


  {
    Heap_t *heap = GetHeap(badaddr);
    MemStack_t *stack = GetStack(badaddr);
    if (heap != NULL)
      printf("   addr part of a heap obj\n");
    if (stack != NULL)
      printf("   addr part of a stack obj\n");
  }
  exit(-1);
}

typedef void (*sa_sigaction_t)(int, struct siginfo*, void*);
struct sigaction old_fpe_action;
void fpe_handler(int signum, 
#if (defined alpha_osf || defined solaris)
		 siginfo_t *siginfo, 
#elif (defined rs_aix)
		 int always_zero,
#endif
		 struct ucontext *uctxt)
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
      raise_exception(uctxt,DivideByZeroExn);
      break;
    case FPE_FLTDIV:
      if (paranoid) 
	printf("Float divide by zero: %d %d ",errno,code);
      raise_exception(uctxt,DivideByZeroExn);
      break;
    case FPE_INTOVF:
      if (paranoid) 
	printf("Integer overflow: we are not getting this... %d %d ",errno,code);
      raise_exception(uctxt,OverflowExn);
      break;
    case FPE_FLTOVF: 
      if (paranoid)
	printf("Float OR integer overflow: %d %d ",errno,code);
      raise_exception(uctxt,OverflowExn);
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
  /*
      printf("Unknown FPE signal in non-alpha not implemented\n");
      */
}



void alarm_handler(int signum, 
#ifdef alpha_osf
		   siginfo_t *siginfo, 
#endif
#ifdef rs_aix
		   int always_zero,
#endif
		   struct ucontext *uctxt)
{
#ifdef alpha_osf
  if (siginfo != 0)
    printf("siginfo for alarm_handler is not nil\n");
#endif
  Interrupt(uctxt);
}

extern int ThreadedVersion;

void signal_init()
{
  buserror_on();
  float_exn_on();
}

void install_signal_handlers(int isMain)
{
  typedef void (*voidhandler_t)();
  struct sigaction newact, oldact;

  sigfillset(&newact.sa_mask);
#if (defined alpha_osf) || (defined solaris)
  newact.sa_flags = SA_SIGINFO | SA_NODEFER;
#endif
#ifdef rs_aix
  newact.sa_flags = 0;
#endif

  if (!isMain) {
    newact.sa_handler = (voidhandler_t) fpe_handler;
    sigaction(SIGFPE,&newact,&old_fpe_action); 
    newact.sa_handler = (voidhandler_t) memfault_handler;
    sigaction(SIGSEGV,&newact,NULL);
    sigaction(SIGBUS,&newact,NULL); 
    sigaction(SIGILL,&newact,NULL);
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

  if (ThreadedVersion)
  {
    struct itimerval newtimer,oldtimer;
    newact.sa_handler = (voidhandler_t) alarm_handler;
    sigaction(SIGVTALRM,&newact,NULL); 
    newtimer.it_interval.tv_sec = 0;
    newtimer.it_interval.tv_usec = 20000;
    newtimer.it_value.tv_sec = 0;
    newtimer.it_value.tv_usec = 20000;
    setitimer(ITIMER_VIRTUAL,&newtimer,&oldtimer); 
/*  setitimer(ITIMER_PROF,&newtimer,&oldtimer); */
  }

}


void float_tester (double arg)
{
  /*  printf("float_tester = address = %d\n",arg); */
  printf("float_tester = %lf\n",arg);
}
