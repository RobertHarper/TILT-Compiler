file /usr5/swasey/t/Release/Bug.sparc.exe
set environment TILT_LIBDIR=/usr5/swasey/t
set height 0

b Stacklet_Alloc

#b Heap_ResetFreshPages
#b Heap_GetMaximumSize
#b Heap_GetSize
#b Heap_GetAvail
#b Heap_GetUsed
#b Heap_Reset
#b Heap_Resize


#
#b GCFromMutator
#b NewStackletFromMutator
#b PopStackletFromMutator
#
#b Finish
#b YieldRest
#b SpawnRest
#b schedulerRest
#b toplevel_exnhandler
##b getOverflowExn
##b getDivExn
#b GC_Gen
#b GCRelease_Gen
#
##b inSomeHeap
##b OverflowFromML
##b DivFromML
###b GCFromML
###b GCFromMutator
##b work
##b NewStackletFromML
##b PopStackletFromML
##b save_regs_MLtoC
#