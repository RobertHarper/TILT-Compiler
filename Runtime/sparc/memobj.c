#include "s.h"
#include "r.h"
#include "sparc.h"
#include <sys/mman.h>
#include <sys/resource.h>

Stacklet_t *Stacklets;
StackChain_t *StackChains;
Heap_t *Heaps;

int NumHeap = 20;
int NumStackChain = 200;	/* 2 * NumThread */
int NumStacklet = 200;	/* NumStackChain */
int NumStackletPerChain = 10;	/* Initial size of a chain's stacklet array. */

/* A user thread heap limit used to indicates that it has been interrupted */
mem_t StopHeapLimit = (mem_t) 1;
/* A user thread heap limit used to indicates that it has not been given space */
mem_t StartHeapLimit = (mem_t) 2;

/* Sizes measured in Kb */
int MLStackletSize = 128;	/* Normal area available to ML */
int CStackletSize = 32;	/* Extra area made available when calling C */
int GuardStackletSize;	/* Guard page at bottom */
int primaryStackletOffset, replicaStackletOffset, stackletOffset;

static const int kilobyte = 1024;

void
my_mprotect(int which, caddr_t bottom, int size, int perm)
{
	int status = mprotect(bottom, size, perm);
	if (status) {
		fprintf(stderr, "mprotect %d failed (%lx,%d,%d): %s\n",
			which,(long)bottom,size,perm,strerror(errno));
		DIE("out of memory");
	}
}

mem_t
my_mmap(int size, int prot)
{
	caddr_t start = NULL;
	static int fd = -1;
	int flags;
	mem_t mapped;
#ifdef sparc
	{
		if (fd == -1)
			if ((fd = open("/dev/zero", O_RDWR)) == -1) {
				perror("/dev/zero");
				DIE("out of memory");
			}
		flags = MAP_PRIVATE;
	}
#else
	flags = MAP_ANONYMOUS;
#endif
#ifdef sparc
	/*
		We'd like to use on-demand allocation if the OS supports it.
		Currently, MAP_NORESERVE is not POSIX.  If other OSs do not
		support it, then we should think about implementing a scheme
		where Heap_t structures are reallocated to allow for growth.
	*/
	flags |= MAP_NORESERVE;
#endif
	mapped = (mem_t) mmap(start, size, prot, flags, fd, 0);
	if (mapped == (mem_t) -1) {
		fprintf(stderr,"mmap %d failed: %s\n", size, strerror(errno));
		DIE("out of memory");
	}
	if (diag)
		fprintf(stderr,"mmap %d succeeded with mapped = %lx, prot = %d\n",
			size, (long)mapped, prot);
	return mapped;
}

void
StackInitialize(void)
{
	int i;
	Stacklets = (Stacklet_t *)emalloc(sizeof(Stacklet_t) * NumStacklet);
	for (i=0; i<NumStacklet; i++) {
		Stacklets[i].count = 0;
		Stacklets[i].mapped = 0;
	}
	StackChains = (StackChain_t *)emalloc(
		sizeof(StackChain_t) * NumStackChain);
	for (i=0; i<NumStackChain; i++) {
		StackChains[i].used = 0;
		StackChains[i].thread = NULL;
		StackChains[i].avail = 0;
		StackChains[i].stacklets = NULL;
	}
}

void
Stacklet_Dealloc(Stacklet_t* stacklet)
{
	assert(stacklet->count > 0);
	if (stacklet->count == 1)
		stacklet->parent = NULL;
	stacklet->count--;
}

void
Stacklet_KillReplica(Stacklet_t* stacklet)
{
	assert(stacklet->count > 0);
	if (stacklet->state == Pending)
		Stacklet_Copy(stacklet);
	while (stacklet->state == Copying)
		;
	CompareAndSwap((int*) &stacklet->state, InactiveCopied, ActiveCopied);
	assert(stacklet->state == Inconsistent
		|| stacklet->state == ActiveCopied);
}

static Stacklet_t*
Stacklet_Alloc(StackChain_t* stackChain)
{
	int i;
	Stacklet_t *res = NULL;
	/*
		Each stacklet contains the primary and replica.  Each one
		starts with a guard page, a C area, and then an ML area.
	*/
	int size = (GuardStackletSize + MLStackletSize + CStackletSize) * kilobyte;	/* for just one of the two: primary and replica */

	assert(stackletOffset == size);
	for (i=0; i<NumStacklet; i++)
		if (CompareAndSwap(&Stacklets[i].count, 0, 1) == 0) {
			res = &Stacklets[i];
			break;
		}
	if (res == NULL)
		DIE("out of stack space");

	res->parent = stackChain;
	res->state = Inconsistent;
	if (!res->mapped) {
		mem_t start = my_mmap(2 * size, PROT_READ | PROT_WRITE);
		mem_t middle = start + size / (sizeof (val_t));

		res->baseExtendedBottom = start +
			(GuardStackletSize * kilobyte) / (sizeof (val_t));
		res->baseBottom = res->baseExtendedBottom +
			(CStackletSize * kilobyte) / (sizeof (val_t));
		res->baseTop = res->baseBottom +
			(MLStackletSize * kilobyte) / (sizeof (val_t));
		assert(res->baseTop == middle);
		/*
			Get some initial room in multiples of 64 bytes; Sparc
			requires at least 68 byte for the save area.
		*/
		res->baseTop -= (128 / sizeof(val_t));
		my_mprotect(0, (caddr_t) start, GuardStackletSize * kilobyte,
			PROT_NONE);	/* Guard page at bottom of primary */
		my_mprotect(1, (caddr_t) middle, GuardStackletSize * kilobyte,
			PROT_NONE);	/* Guard page at bottom of replica */

		res->callinfoStack = SetCreate(size / (32 * sizeof (val_t)));
		res->mapped = 1;
	}
	res->baseCursor = res->baseTop;
	for (i=0; i<32; i++)
		res->bottomBaseRegs[i] = 0;
	SetReset(res->callinfoStack);
	return res;
}

int
StackletId(Stacklet_t* stacklet)
{
	return (int)(stacklet - Stacklets);
}

mem_t
StackletPrimaryTop(Stacklet_t* stacklet)
{
	return stacklet->baseTop + (primaryStackletOffset / sizeof(val_t));
}

mem_t
StackletPrimaryCursor(Stacklet_t* stacklet)
{
	return stacklet->baseCursor + (primaryStackletOffset / sizeof(val_t));
}

mem_t
StackletPrimaryBottom(Stacklet_t* stacklet)
{
	return stacklet->baseBottom + (primaryStackletOffset / sizeof(val_t));
}

Stacklet_t*
NewStacklet(StackChain_t* stackChain)
{
	int i;
	Stacklet_t *newStacklet = Stacklet_Alloc(stackChain);
	if (stackChain->cursor + 1 == stackChain->avail) {
		int avail = stackChain->avail * 2;
		assert(avail > 0);
		stackChain->stacklets = (Stacklet_t**) erealloc(
			stackChain->stacklets, sizeof(Stacklet_t*) * avail);
		for (i=stackChain->avail; i < avail; i++) {
			stackChain->stacklets[i] = NULL;
		}
		stackChain->avail = avail;
	}
	stackChain->stacklets[stackChain->cursor++] = newStacklet;
	for (i=0; i<stackChain->cursor; i++)
		assert(stackChain->stacklets[i]->count > 0);
	return newStacklet;
}

/* Copy primary into replica */
int
Stacklet_Copy(Stacklet_t* stacklet)
{
	StackletState_t state;

	state = CompareAndSwap((int*) &stacklet->state, Pending, Copying);
	if (state == Pending) {
		int i;
		int activeSize = (stacklet->baseTop - stacklet->baseCursor) *
			sizeof(val_t);
		mem_t primaryCursor = stacklet->baseCursor +
			(primaryStackletOffset / sizeof(val_t));
		mem_t replicaCursor = stacklet->baseCursor +
			(replicaStackletOffset / sizeof(val_t));
		volatile reg_t* primaryRegs = &stacklet->bottomBaseRegs
			[primaryStackletOffset == 0 ? 0 : 32];
		volatile reg_t* replicaRegs = &stacklet->bottomBaseRegs
			[primaryStackletOffset == 0 ? 32 : 0];
		assert(stacklet->count > 0);
		assert(stacklet->baseExtendedBottom <= stacklet->baseCursor);
		assert(stacklet->baseCursor <= stacklet->baseTop);
		SetReset(stacklet->callinfoStack);
		stacklet->replicaCursor = stacklet->baseCursor;
		stacklet->replicaRetadd = stacklet->retadd;
		memcpy(replicaCursor, primaryCursor, activeSize);
		for (i=0; i<32; i++)
			replicaRegs[i] = primaryRegs[i];
		stacklet->state = InactiveCopied;
		return 1;
	}
	while (stacklet->state == Copying)
		;
	assert(stacklet->state != Inconsistent);
	assert(stacklet->state != Pending);
	return 0;
}

/*
	Given the new sp, establish the most recent stacklet.  Return the
	most recent stacklet.
*/
Stacklet_t*
EstablishStacklet(StackChain_t* stackChain, mem_t sp)
{
	int i,active;
	mem_t sp_base = sp - primaryStackletOffset / sizeof(val_t);
	for (active=stackChain->cursor-1; active>=0; active--) {
		Stacklet_t *stacklet = stackChain->stacklets[active];
		if (stacklet->baseExtendedBottom <= sp_base
		&& sp_base <= stacklet->baseTop) {
			stacklet->baseCursor = sp_base;
			break;
		}
	}
	if (active < 0) {
		fprintf(stderr,"EstablishStacklet failed for sp = %lx  sp_base = %lx",
			(long)sp,(long)sp_base);
		for (i=0; i < stackChain->cursor; i++) {
			Stacklet_t *stacklet = stackChain->stacklets[i];
			fprintf(stderr,"Stacklet %d: %lx to %lx\n", i,
				(long)stacklet->baseExtendedBottom,
				(long)stacklet->baseTop);
		}
		DIE("stack pointer not in stack chain");
	}
	for (i=active+1; i<stackChain->cursor; i++) {
		Stacklet_Dealloc(stackChain->stacklets[i]);
		stackChain->stacklets[i] = NULL;
	}
	stackChain->cursor = active + 1;
	return stackChain->stacklets[active];
}

/* Deallocate most recent stacklet - at least one active must remain */
void
PopStacklet(StackChain_t* stackChain)
{
	int active = stackChain->cursor - 1;
	Stacklet_Dealloc(stackChain->stacklets[active]);
	stackChain->stacklets[active] = NULL;
	stackChain->cursor--;
	assert(stackChain->cursor >= 0);
}

/* Remove oldest stacklet from stack chain */
void
DequeueStacklet(StackChain_t* stackChain)
{
	int i;
	Stacklet_Dealloc(stackChain->stacklets[0]);
	for (i=1; i<stackChain->cursor; i++) {
		stackChain->stacklets[i-1] = stackChain->stacklets[i];
		assert(stackChain->stacklets[i-1]->count > 0);
	}
	stackChain->stacklets[stackChain->cursor-1] = NULL;
	stackChain->cursor--;
	assert(stackChain->cursor>=0);
}

StackChain_t*
StackChain_BaseAlloc(Thread_t* t, int n)
{
	int i;
	for (i=0; i<NumStackChain; i++)
		if (CompareAndSwap(&StackChains[i].used, 0, 1) == 0) {
			StackChain_t *res = &StackChains[i];
			/* memBarrier(); */
			assert(res->used == 1);
			res->cursor = 0;
			res->thread = t;
			res->avail = n;
			assert(n > 0);
			res->stacklets = (Stacklet_t**)emalloc(sizeof(Stacklet_t*) * n);
			for (i=0; i < n; i++) {
				res->stacklets[i] = NULL;
			}
			return res;
		}
	DIE("could not allocate new stack chain\n");
	return 0;	/* NOTREACHED */
}

StackChain_t*
StackChain_Copy(void* tVoid, StackChain_t* src)
{
	int i;
	Thread_t *t = (Thread_t *) tVoid;
	StackChain_t* res = StackChain_BaseAlloc(t, src->avail);
	assert(t == t->stack->thread);
	assert(src->used);
	res->cursor = src->cursor;
	for (i=0; i<src->cursor; i++) {
		res->stacklets[i] = src->stacklets[i];
		src->stacklets[i]->count++;
	}
	return res;
}

int
StackChain_Size(StackChain_t* chain)
{
	int i, sum = 0;
	for (i=0; i<chain->cursor; i++) {
		Stacklet_t *stacklet = chain->stacklets[i];
		sum += (stacklet->baseTop - stacklet->baseCursor)
			* sizeof(val_t);
	}
	return sum;
}

StackChain_t*
StackChain_Alloc(void* tVoid)
{
	Thread_t *t = (Thread_t *) tVoid;
	StackChain_t* res = StackChain_BaseAlloc(t, NumStackletPerChain);
	NewStacklet(res);
	return res;
}

void
StackChain_Dealloc(StackChain_t* stackChain)
{
	int i;
	for (i=0; i<stackChain->cursor; i++) {
		Stacklet_Dealloc(stackChain->stacklets[i]);
		stackChain->stacklets[i] = NULL;
	}
	stackChain->cursor = 0;
	stackChain->thread = NULL;
	stackChain->used = 0;	/* Should be last */
}

Stacklet_t*
GetStacklet(mem_t add)
{
	int i;
	mem_t offsetAdd = add - stackletOffset / sizeof(val_t);
	for (i=0; i<NumStacklet; i++) {
		Stacklet_t *s = &Stacklets[i];
		if (s->count
		&& ((s->baseExtendedBottom <= add && add <= s->baseTop)
			|| (s->baseExtendedBottom <= offsetAdd
				&& offsetAdd <= s->baseTop)))
			return s;
	}
	return NULL;
}

Stacklet_t*
CurrentStacklet(StackChain_t* stackChain)
{
	Stacklet_t *stacklet = stackChain->stacklets[stackChain->cursor-1];
	assert(stackChain->used);
	assert(stackChain->cursor > 0
		&& stackChain->cursor < stackChain->avail);
	assert(stacklet->count > 0);
	return stacklet;
}

void
HeapInitialize(void)
{
	int i;
	Heaps = (Heap_t *)emalloc(sizeof(Heap_t) * NumHeap);
	for (i=0; i<NumHeap; i++) {
		Heap_t *heap = &(Heaps[i]);
		heap->id = i;
		heap->valid = 0;
		heap->top = 0;
		heap->bottom = 0;
		heap->cursor = 0;
		heap->mappedTop = 0;
		heap->writeableTop = 0;
		heap->lock = (pthread_mutex_t *)emalloc(sizeof(pthread_mutex_t));
		pthread_mutex_init(heap->lock,NULL);
	}
}

void
PadHeapArea(mem_t bottom, mem_t top)
{
	if (bottom > top)
		DIE("PadHeapArea failing beause bottom > top");
	if (bottom < top)
		*bottom = MAKE_SKIP(top - bottom);
}

Heap_t*
GetHeap(mem_t add)
{
	int i;
	for (i=0; i<NumHeap; i++)
		if (Heaps[i].valid && Heaps[i].id==i
		&& Heaps[i].bottom <= add
		&& Heaps[i].top >= add)
			return (Heaps+i);
	return NULL;
}

int
inSomeHeap(ptr_t v)
{
	int i;
	for (i=0; i < NumHeap; i++) {
		if (Heaps[i].valid && Heaps[i].id==i && inHeap(v, &Heaps[i]))
			return 1;
	}
	return 0;
}

void
SetRange(range_t *range, mem_t low, mem_t high)
{
	range->low = low;
	range->high = high;
	range->diff = (high - low) * (sizeof (val_t));
}

Heap_t*
Heap_Alloc(int MinSize, int MaxSize)
{
	static int heap_count = 0;
	Heap_t *res = &(Heaps[heap_count++]);
	int maxsize_pageround = RoundUp(MaxSize,TILT_PAGESIZE);
	res->size = maxsize_pageround;
	res->bottom = (mem_t) my_mmap(maxsize_pageround,
		PROT_READ | PROT_WRITE);
	res->cursor = res->bottom;
	res->top = res->bottom + MinSize / (sizeof (val_t));
	res->writeableTop = res->bottom +
		maxsize_pageround / (sizeof (val_t));
	res->mappedTop = res->writeableTop;
	SetRange(&(res->range), res->bottom, res->mappedTop);
	res->valid = 1;
	res->bitmap = paranoid ? CreateBitmap(maxsize_pageround / 4) : NULL;
	res->freshPages = (int *)emalloc(
		DivideUp(maxsize_pageround / TILT_PAGESIZE, 32) * sizeof(int));
	memset((int *)res->freshPages, 0,
		DivideUp(maxsize_pageround / TILT_PAGESIZE, 32) * sizeof(int));
	assert(res->bottom != (mem_t) -1);
	assert(heap_count < NumHeap);
	assert(MaxSize >= MinSize);
	/*
		Try to lock down pages and force page-table to be initialized;
		otherwise, PadHeapArea can often take 0.1 - 0.2 ms.  Even with
		this, there are occasional (but far fewer) page table misses.
	*/
	if (geteuid() == 0)
		(void)mlock((caddr_t) res->bottom, maxsize_pageround);
	return res;
}

int
Heap_ResetFreshPages(Proc_t* proc, Heap_t* h)
{
	int MaxSize = sizeof(val_t) * (h->mappedTop - h->bottom);
	int pages = DivideUp(MaxSize,TILT_PAGESIZE);
	memset((int *)h->freshPages, 0, DivideUp(pages, 32) * sizeof(int));
	return pages;
}

int
Heap_GetMaximumSize(Heap_t* h)
{
	return (sizeof (val_t)) * (h->mappedTop - h->bottom);
}

int
Heap_GetSize(Heap_t* h)
{
	return (sizeof (val_t)) * (h->top - h->bottom);
}

int
Heap_GetAvail(Heap_t* h)
{
	return (sizeof (val_t)) * (h->top - h->cursor);
}

int
Heap_GetUsed(Heap_t* h)
{
	return (sizeof (val_t)) * (h->cursor - h->bottom);
}

void
Heap_Reset(Heap_t* h)
{
	h->cursor = h->bottom;
}

void
Heap_Resize(Heap_t* h, long newSize, int reset)
{
	long usedSize;
	long maxSize = (h->mappedTop - h->bottom) * sizeof(val_t);
	long oldWriteableSize = (h->writeableTop - h->bottom) * sizeof(val_t);
	long newSizeRound = RoundUp(newSize, TILT_PAGESIZE);

	if (newSize > maxSize) DIE("resized heap too big");
	if (reset) {
		h->cursor = h->bottom;
	}
	usedSize = (h->cursor - h->bottom) * sizeof(val_t);
	assert(usedSize <= newSize);
	h->top = h->bottom + (newSize / sizeof(val_t));

	if (newSizeRound > oldWriteableSize) {
		my_mprotect(6,(caddr_t) h->writeableTop,
			newSizeRound - oldWriteableSize,
			PROT_READ | PROT_WRITE);
		h->writeableTop = h->bottom + newSizeRound / sizeof(val_t);
	}
	else if (paranoid && newSizeRound < oldWriteableSize) {
		my_mprotect(7,(caddr_t) (h->bottom +
			newSizeRound / sizeof(val_t)),
			oldWriteableSize - newSizeRound, PROT_NONE);
		h->writeableTop = h->bottom + newSizeRound / sizeof(val_t);
	}
	assert(h->bottom <= h->cursor);
	assert(h->cursor <= h->top);
	assert(h->top <= h->writeableTop);
	assert(h->writeableTop <= h->mappedTop);
}

/*
	Why is the type wrong?  Why not return (to the signal handler) for
	further diagnostics?
*/
mem_t
StackError(ucontext_t* ucontext, mem_t badadd)
{
	mem_t sp = GetSp(ucontext);

	fprintf(stderr,"\n------------------StackError---------------------\n");
	fprintf(stderr,"sp, badreference:  %lx   %lx\n",(long)sp,(long)badadd);
	DIE("stack error");
	return 0; /* NOTREACHED */
}

static unsigned long
rlimit(int resource)
{
	struct rlimit r;
	int e;
	unsigned long unlimited = (unsigned long)INT_MAX;

	e = getrlimit(resource,&r);
	if(e==-1)
		return unlimited;
	if(r.rlim_max == RLIM_INFINITY)
		return unlimited;
	return ((unsigned long)(int)r.rlim_max) * 0.90;
}

void
memobj_init(void)
{
	unsigned long min_bytes, max_bytes;
	unsigned long mem_bytes = GetPhysicalPages() * 0.90 * TILT_PAGESIZE;
	unsigned long cache_bytes = GetBcacheSize() * 2;

	min_bytes = 2048 * 1024;
	min_bytes = Max(min_bytes, cache_bytes);

	max_bytes = (unsigned long)INT_MAX;
	max_bytes = Min(max_bytes, mem_bytes);
	max_bytes = Min(max_bytes, rlimit(RLIMIT_DATA));
	max_bytes = Min(max_bytes, rlimit(RLIMIT_AS));
#ifdef RLIMIT_VMEM
	max_bytes = Min(max_bytes, rlimit(RLIMIT_VMEM));
#endif

	init_int(&MinHeapByte, min_bytes);
	init_int(&MaxHeapByte, 0.40 * max_bytes);
	assert(MinHeapByte <= MaxHeapByte);

#ifdef sparc
	assert(TILT_PAGESIZE == sysconf(_SC_PAGESIZE));
#else
	assert(TILT_PAGESIZE == sysconf(_SC_PAGE_SIZE));
#endif
	StackInitialize();
	HeapInitialize();
	GuardStackletSize = TILT_PAGESIZE / kilobyte;
	stackletOffset = (GuardStackletSize + MLStackletSize + CStackletSize) *
		kilobyte;
	primaryStackletOffset = 0;
	replicaStackletOffset = stackletOffset;
	/* So we don't pay mmap for first thread - general case? XXXX */
	{ 
		int i;
		Stacklet_t *temp[5];
		for (i=0; i<5; i++)
			temp[i] = Stacklet_Alloc(NULL);
		for (i=0; i<5; i++)
			Stacklet_Dealloc(temp[i]);
	}
}
