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
#include <signal.h>
#include "thread.h"

#ifdef alpha_osf
#include <siginfo.h>
#include <machine/fpu.h>
#endif
#ifdef rs_aix
#include <fptrap.h> 
#endif

#define WRITE
#undef gprintf
#ifndef SUPPRESS
#define gprintf printf
#else
#define gprintf
#endif

#ifdef alpha_osf
long GetPc(struct ucontext *uctxt)          { return (uctxt->uc_mcontext.sc_pc); }
long GetSp(struct ucontext *uctxt)          { return (uctxt->uc_mcontext.sc_sp); }
long GetIReg(struct ucontext *uctxt, int i) { return (uctxt->uc_mcontext.sc_regs[i]); }
void SetIReg(struct ucontext *uctxt, int i, long v) { uctxt->uc_mcontext.sc_regs[i] = v; }

long GetBadAddr(struct ucontext *uctxt, 
		 siginfo_t *siginfo)   { return (long)(siginfo->si_addr); }
#endif


#ifdef rs_aix
scp is sigcontext obtained from uctxt
long *GetPc(struct ucontext *uctxt)    { return &((scp)->sc_jmpbuf.jmp_context.iar); }
long *GetSp(struct ucontext *uctxt)    { return &((scp)->sc_jmpbuf.jmp_context.gpr[1]); }
long *GetIRegs(struct ucontext *uctxt) { return &((scp)->sc_jmpbuf.jmp_context.gpr[0]); }
long  GetBadAddr(struct ucontext *uctxt, int dummy) { return &((scp)->sc_jmpbuf.jmp_context.o_vaddr); }
#endif

#ifdef solaris
long GetIReg(struct ucontext *uctxt, int i)    
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
      int *sp = uctxt->uc_mcontext.gregs[REG_SP];
      return sp[i - 16];
    }
  }
  assert(0);
}

void SetIReg(struct ucontext *uctxt, int i, long v)    
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
long GetPc(struct ucontext *uctxt)    { return uctxt->uc_mcontext.gregs[REG_PC]; }
long GetSp(struct ucontext *uctxt)    { return GetIReg(uctxt,SP_REG); }
long GetBadAddr(struct ucontext *uctxt, 
		 siginfo_t *siginfo)   { return (long)(siginfo->si_addr); }
#endif

void GetIRegs(struct ucontext *uctxt,long *dest) 
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
void float_exn_on()
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
void float_exn_on()
{
  fp_enable_all();
}
#endif
#ifdef solaris
void float_exn_on()
{
  printf("need to implemented float_exn_on for SPARC");
}
#endif

#ifdef alpha_osf
void buserror_on()
{
  int buf[2], error;
  
  /* Deliver a SIGBUS signal and don't print the warning */
  buf[0] = SSIN_UACPROC;
  buf[1] = UAC_SIGBUS | UAC_NOPRINT;
  error = setsysinfo(SSI_NVPAIRS,buf, 1, 0, 0);
  assert(!error);
}
#else
void buserror_on()
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
  SysThread_t *sth = getSysThread();
#if (defined alpha_osf) || (defined solaris)
  int code = siginfo->si_code;
#elif (defined rs_aix)
  siginfo_t *siginfo = NULL;
  int code = 0;
#endif
  int badaddr = GetBadAddr(uctxt,siginfo);
  int badpc = GetPc(uctxt);

#if (defined alpha_osf) || (defined solaris)
  assert(signum == (siginfo->si_signo));
#endif

  printf("SysThread %d:  Memory error at %d with PC = %d\n",sth->stid, badaddr, badpc);
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
	  printf("SEGV_MAPERR  address %d not mapped\n",sth->stid,badaddr);
	  break;
	case SEGV_ACCERR:
	  printf("SEGV_ACCERR  invalid permissions for address %d\n",
		 sth->stid,badaddr);
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
	    printf("BUS_ADRALN invalid address alignment\n",sth->stid);
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

#ifdef WRITE
  {
    extern value_t writelist_start;
    extern value_t writelist_cursor;    
    extern value_t writelist_end;
    if ((value_t)badaddr == ((value_t)writelist_end)+sizeof(value_t))
      { 
	printf("past writelist.  XXX need to implement resize.\n");
	printf("writelist_start = %d\n",writelist_start);
	printf("writelist_cursor = %d\n",writelist_cursor);
	printf("writelist_end = %d\n",writelist_end);
      }
  }
#endif
  {
    Heap_t *heap = GetHeap(badaddr);
    Stack_t *stack = GetStack(badaddr);
    if (heap != NULL)
      printf("   addr part of a heap obj\n");
    if (stack != NULL)
      printf("   addr part of a stack obj\n");
  }
  exit(-1);
}


void fpe_handler(int signum, 
#ifdef alpha_osf
		 siginfo_t *siginfo, 
#endif
#ifdef solaris
		 siginfo_t *siginfo, 
#endif
#ifdef rs_aix
		 int always_zero,
#endif
		 struct ucontext *uctxt)
{

  int signo = siginfo->si_signo;
  int errno = siginfo->si_errno;
  int code = siginfo->si_code;
  if (signum != SIGFPE || signo != SIGFPE)
    printf("BUG: fpe_handler for non fpe signal\n");
  switch (code)
    {
    case FPE_INTDIV:
      gprintf("%d %d ",errno,code);
      gprintf("Integer divide by zero\n");
      raise_exception(uctxt,divide_exn);
      break;
    case FPE_FLTDIV:
      gprintf("%d %d ",errno,code);
      gprintf("Float   divide by zero\n");
      raise_exception(uctxt,divide_exn);
      break;
    case FPE_INTOVF:
      gprintf("%d %d ",errno,code);
      gprintf("Integer overflow: too bad we never get this\n");
      raise_exception(uctxt,overflow_exn);
      break;
    case FPE_FLTOVF:
      gprintf("%d %d ",errno,code);
      gprintf("Float overflow: or could be integer overflow\n");
      raise_exception(uctxt,overflow_exn);
      break;
    case FPE_FLTUND:
      printf("%d %d ",errno,code);
      printf("Float Underflow\n");
      break;
    case FPE_FLTRES:
      printf("%d %d ",errno,code);
      printf("Float Inexact result\n");
      break;
    case FPE_FLTINV:
      printf("%d %d ",errno,code);
      printf("Float Invalid operation\n");
      return;
      break;
    case FPE_FLTSUB:
      printf("%d %d ",errno,code);
      printf("Float Subscript out of range\n");
      break;
    default:
      printf("%d %d ",errno,code);
      printf("Unknown FPE signal\n");
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
  typedef void (*voidhandler_t)();
  struct sigaction newact, oldact;

  buserror_on();
  float_exn_on();

  sigfillset(&newact.sa_mask);
#ifdef alpha_osf
  newact.sa_flags = SA_SIGINFO | SA_NODEFER | SA_ONSTACK;
#endif
#ifdef rs_aix
  newact.sa_flags = 0;
#endif

  newact.sa_handler = (voidhandler_t) fpe_handler;
  sigaction(SIGFPE,&newact,&oldact);

  newact.sa_handler = (voidhandler_t) memfault_handler;
  sigaction(SIGSEGV,&newact,&oldact);
  sigaction(SIGBUS,&newact,&oldact); 
  sigaction(SIGILL,&newact,&oldact);

  newact.sa_handler = (voidhandler_t) alarm_handler;
  sigaction(SIGVTALRM,&newact,&oldact); 

  /* install a stack for signal handlers */
#ifdef alpha_osf
  {
    struct sigstack in;
    struct sigstack out;
    long temp[4096];
    in.ss_sp = (void *) ((long)temp+4096);
    in.ss_onstack = 0;
    sigstack(&in,&out);
  }
#endif

  if (ThreadedVersion)
  {
    struct itimerval newtimer,oldtimer;
    newtimer.it_interval.tv_sec = 0;
    newtimer.it_interval.tv_usec = 20000;
    newtimer.it_value.tv_sec = 0;
    newtimer.it_value.tv_usec = 20000;
    setitimer(ITIMER_VIRTUAL,&newtimer,&oldtimer); 
/*    setitimer(ITIMER_PROF,&newtimer,&oldtimer); */
  }


  /*     signaltest();   */
}


float_tester (double arg)
{
  /*  printf("float_tester = address = %d\n",arg); */
  printf("float_tester = %lf\n",arg);
}
