/* Must agree with sparc.h. */
#define MLsaveregs_disp	0
#define proc_disp	MLsaveregs_disp + 4*32 + 8*32
#define notinml_disp	proc_disp + 4
#define safetogc_disp	notinml_disp + 4
#define scratch_disp	safetogc_disp + 4 + 4
#define request_disp	scratch_disp + 8
#define requestInfo_disp	request_disp + 4
#define Csaveregs_disp	requestInfo_disp + 4
#define writelistAlloc_disp	Csaveregs_disp + 4*32 + 8*32
#define writelistLimit_disp	writelistAlloc_disp + 4
#define stackLimit_disp	writelistLimit_disp + 4
#define stackTop_disp	stackLimit_disp + 4

#define NoRequest	0
#define YieldRequest	1
#define StartRequest	2
#define GCRequestFromML	3
#define GCRequestFromC	4
#define MajorGCRequestFromC	5
