/* Must agree with Thread_t. */
#define MLsaveregs_disp	0
#define proc_disp	longSz*32+8*32
#define notinml_disp	longSz*32+8*32+CptrSz
#define scratch_disp	longSz*32+8*32+CptrSz+longSz
#define request_disp	longSz*32+8*32+CptrSz+longSz+doubleSz
#define requestInfo_disp	longSz*32+8*32+CptrSz+longSz+doubleSz+longSz
#define Csaveregs_disp	longSz*32+8*32+CptrSz+longSz+doubleSz+longSz+longSz
#define writelistAlloc_disp	longSz*32+8*32+CptrSz+longSz+doubleSz+longSz+longSz+32*longSz+32*doubleSz
#define writelistLimit_disp	longSz*32+8*32+CptrSz+longSz+doubleSz+longSz+longSz+32*longSz+32*doubleSz+MLptrSz
#define stackLimit_disp	longSz*32+8*32+CptrSz+longSz+doubleSz+longSz+longSz+32*longSz+32*doubleSz+MLptrSz+MLptrSz
#define stackTop_disp	longSz*32+8*32+CptrSz+longSz+doubleSz+longSz+longSz+32*longSz+32*doubleSz+MLptrSz+MLptrSz+intSz

#define NoRequest	0
#define YieldRequest	1
#define StartRequest	2
#define GCRequestFromML	3
#define GCRequestFromC	4
#define MajorGCRequestFromC	5
