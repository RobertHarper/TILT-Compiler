#include "s.h"
#include "r.h"
#include "sparc.h"

extern void raise_exception_raw(Thread_t*, exn);

/*
	Counter for ML exception stamps; used by ML code.
*/
uint32 exncounter = 4000;

void
exn_init(void)
{
}

void
raise_exn(exn exn)
{
	Thread_t* th = getThread();
	raise_exception_raw(th, exn);
}

void
raise_exception(ucontext_t* uctxt, exn exn)
{
	Thread_t* th = getThread();

	if (th == NULL)
		/*
			This happens, for example, if the runtime
			divides by zero during initialization.
		*/
		DIE("spurious exception");

	if (!th->notInML) {
		/*
			Move saved register from uctxt to thread area
			so it can be restored by raise_exception_raw.
		*/
		GetIRegs(uctxt, (unsigned long *) th->saveregs);
	}

	if (debug) {
		int i;
		ptr_t exn_ptr = (ptr_t) th->saveregs[EXNPTR];
		val_t code = get_record(exn_ptr,0);

		fprintf(stderr,"\n\n--------raise_exception entered---------\n");
		fprintf(stderr,"raise: exn_ptr is %lx\n",(long)exn_ptr);
		fprintf(stderr,"raise: rec[-1] is %d\n",((int *)exn_ptr)[-1]);
		fprintf(stderr,"raise: rec[0] is %d\n",((int *)exn_ptr)[0]);
		for (i=0; i<32; i++)
			fprintf(stderr,"RAISE: saveregs[%d] is %ld\n",i,th->saveregs[i]);
		fprintf(stderr,"jumping to raise_exception_raw\n");
		fflush(stderr);
	}

	raise_exception_raw(th,exn);
}

void
toplevel_exnhandler(Thread_t* th)
{
	volatile unsigned long *saveregs = th->saveregs;
	exn exn = (ptr_t)saveregs[EXNARG];
	string msg = exnMessageRuntime(exn);
	int msg_len = stringlen(msg);
	char* msg_buf = stringbuf(msg);

	if(collector_type == Semispace || collector_type == Generational)
		fprintf(stderr,"uncaught exception: %.*s\n", msg_len, msg_buf);
	else
		fprintf(stderr,"Proc %d: Thread %d (%d): uncaught exception: %.*s\n",
			getProc()->procid, th->tid, th->id, msg_len, msg_buf);
	fflush(stderr);
	Finish();
}

void
RestoreStackFromMutator(Thread_t* th)
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

/*
	Project Div and Overflow from ../../Basis/tiltexn.sml.
*/
#ifdef FLATTEN_MODULES

#	define Divmk	ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTDiv__0_DOTmk__i__INT
#	define Ovflmk	ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTOverflow__0_DOTmk__i__INT

	exn
	getOverflowExn(void)
	{
		return (ptr_t) GetGlobal(&Ovflmk);
	}

	exn
	getDivExn(void)
	{
		return (ptr_t) GetGlobal(&Divmk);
	}

#else
#	define TiltExn	ml__PLUSUTiltExn__INT__r__INT

	enum {
		/* The declarations in ../../Basis/tiltexn.sml. */
		Div=0, Overflow,

		/*
			The components of an exception module.
			NB must agree with ../../Elaborator/toil.sml.
		*/
		Stamp=0, Mk,
	};

	exn
	getOverflowExn(void)
	{
		ptr_t compunit = (ptr_t) GetGlobal(&TiltExn);
		ptr_t exnmod = (ptr_t) get_record(compunit,Overflow);
		return (ptr_t) get_record(exnmod, Mk);
	}

	exn
	getDivExn(void)
	{
		ptr_t compunit = (ptr_t) GetGlobal(&TiltExn);
		ptr_t exnmod = (ptr_t) get_record(compunit,Div);
		return (ptr_t) get_record(exnmod, Mk);
	}
#endif
