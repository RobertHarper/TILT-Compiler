/* ------------------------------ Helper Routines -------------------- */
void AlignMemoryPointer(mem_t *allocRef, Align_t align)
{
  int curEven = (((val_t)(*allocRef)) & 7) == 0;
  if ((align == OddWordAlign && curEven) ||
      (align == EvenWordAlign && !curEven))
    *((*allocRef)++) = MAKE_SKIP(1);
}
	
000012dc <AlignMemoryPointer> ld  [ %o0 ], %g2
000012e0 <AlignMemoryPointer+4> and  %g2, 7, %g2
000012e4 <AlignMemoryPointer+8> cmp  %g0, %g2
000012e8 <AlignMemoryPointer+c> subc  %g0, -1, %g2
000012ec <AlignMemoryPointer+10> cmp  %o1, 1
000012f0 <AlignMemoryPointer+14> bne  00001308 <AlignMemoryPointer+2c>
000012f4 <AlignMemoryPointer+18> cmp  %o1, 2
000012f8 <AlignMemoryPointer+1c> cmp  %g2, 0
000012fc <AlignMemoryPointer+20> bne,a   0000131c <AlignMemoryPointer+40>
00001300 <AlignMemoryPointer+24> ld  [ %o0 ], %g2
00001304 <AlignMemoryPointer+28> cmp  %o1, 2
00001308 <AlignMemoryPointer+2c> bne  0000132c <AlignMemoryPointer+50>
0000130c <AlignMemoryPointer+30> cmp  %g2, 0
00001310 <AlignMemoryPointer+34> bne  0000132c <AlignMemoryPointer+50>
00001314 <AlignMemoryPointer+38> nop 
00001318 <AlignMemoryPointer+3c> ld  [ %o0 ], %g2
0000131c <AlignMemoryPointer+40> mov  0x17, %g3
00001320 <AlignMemoryPointer+44> st  %g3, [ %g2 ]
00001324 <AlignMemoryPointer+48> add  %g2, 4, %g2
00001328 <AlignMemoryPointer+4c> st  %g2, [ %o0 ]
0000132c <AlignMemoryPointer+50> retl 
00001330 <AlignMemoryPointer+54> nop 
