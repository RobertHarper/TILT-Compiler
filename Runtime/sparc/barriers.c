#include "s.h"
#include "r.h"
#include "sparc.h"

Barriers_t*
createBarriers(int numProc, int maxBarrier)
{
	int i, j;
	Barriers_t *b = (Barriers_t *) emalloc(sizeof(Barriers_t));
	b->numProcessor = numProc;
	b->maxBarrier = maxBarrier;
	b->phase = maxBarrier;
	b->lastUsed = maxBarrier - 1;
	assert(b->phase >= 3); /* Must have 3 strong barriers. */
	b->barriers = (long *) ecalloc(b->maxBarrier, sizeof(long));
	b->index = (volatile long **) ecalloc(b->maxBarrier, sizeof(long *));
	for (i=0; i<b->maxBarrier; i++) {
		b->index[i] = (long *) ecalloc(b->numProcessor, sizeof(long));
		for (j=0; j<b->numProcessor; j++)
			b->index[i][j] = -1;
	}
	return b;
}

int
strongBarrier(Barriers_t* b, Proc_t* proc)
{
	ProcessorState_t prevState = proc->state;
	int i, stalled = 0;
	int which = proc->barrierPhase;
	int index = FetchAndAdd(&b->barriers[which], 1);
	proc->barrierPhase = (which + 1) % b->phase;
	if (collectDiag >= 2)
		printf("Proc %d: Waiting at strong barrier %d.  %ld more processors to pass.\n", 
			proc->procid, which, b->numProcessor - b->barriers[which]);
	b->index[which][index] = proc->procid;
	while (b->barriers[which] < b->numProcessor) {
		if (!stalled) {
			stalled = 1;
			procChangeState(proc, GCIdle, 500);
		}
		memBarrier();
	}
	if (stalled)
		procChangeState(proc, prevState, 0);
	if (collectDiag >= 2)
		printf("Proc %d: Passing strong barrier %d\n", proc->procid, which);
	assert(b->barriers[which] == b->numProcessor);
	i = b->lastUsed;
	while (i != which) {
		b->barriers[i] = 0;	/* Reset counters */
		i = (i + 1) % b->phase;
	}
	b->lastUsed = which;
	memBarrier();

	return index;
}

int
weakBarrier(Barriers_t* b, Proc_t* proc)
{
	int which = proc->barrierPhase;
	int index = FetchAndAdd(&b->barriers[which], 1);
	proc->barrierPhase = (which + 1) % b->phase;
	if (collectDiag >= 2)
		printf("Proc %d: Passing weak barrier %d.  %ld more processors to pass\n", 
			proc->procid, which, b->numProcessor - b->barriers[which]);
	b->index[which][index] = proc->procid;
	memBarrier();
	return index;
}

int
checkBarrier(Barriers_t* b, Proc_t* proc)
{
	int which = proc->barrierPhase;
	memBarrier();
	return (int) b->barriers[which];
}
