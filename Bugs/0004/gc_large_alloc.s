
	;; Possible problems:	
	;; * Someone is trashing our registers
	;; * This (AND -4) stuff is problematic (should be shifts)
	
mem_t gc_large_alloc(Proc_t *proc, int byteLen, Align_t align)
{
  mem_t region, end;
  int bitmapPos;
  int padByteLen = byteLen + ((align == NoWordAlign) ? 0 : 4);
  int chunksNeeded = DivideUp(padByteLen,largebitmapsize);

  assert(byteLen >= 1024);
  bitmapPos = AllocBitmapRange(allocMap,chunksNeeded);
  if (bitmapPos < 0)
    return NULL; /* allocation failed */

  region = largeSpace->bottom + (largebitmapsize * bitmapPos) / (sizeof (val_t));
  end = region + (chunksNeeded * largebitmapsize) / (sizeof (val_t));
  AlignMemoryPointer(&region, align);
  PadHeapArea(region + byteLen / sizeof(val_t), end);
  return region;
}

;;; Rough register usage:
	
	;; %i0 varies:	data seg base, region
	;; %i1 fixed:	int byteLen
	;; %i2 fixed:	Align_t align

	;; %l0 varies:	largeSpace, region, end
	;; %l1 fixed:	chunksNeeded

	;; %o0 temporary
	;; %o1 temporary
	;; %o2 varies:	largeSpace->bottom, region
	;; %o3 printf/assert arg
	
0x7047a8 <gc_large_alloc>:      save  %sp, -120, %sp		; Setup new stack frame
								; %sp = old%sp - 120
								; %fp = old%sp
	
	;;   int padByteLen = byteLen + ((align == NoWordAlign) ? 0 : 4);
	
0x7047ac <gc_large_alloc+4>:    cmp  %i2, 0			; compare (align, NoWordAlign)
0x7047b0 <gc_large_alloc+8>:    be  0x7047bc <gc_large_alloc+20>
0x7047b4 <gc_large_alloc+12>:   mov  %i1, %o0			; equal,     o0 = byteLen
0x7047b8 <gc_large_alloc+16>:   add  %i1, 4, %o0		; not equal, o0 = byteLen + 4
	
	;; #define DivideUp(a,div) (((a) + (div) - 1) / (div))
	;;   int chunksNeeded = DivideUp(padByteLen,largebitmapsize)

0x7047bc <gc_large_alloc+20>:   sethi  %hi(0x959c00), %i0	; setup for read from data segment
0x7047c0 <gc_large_alloc+24>:   ld  [ %i0 + 0x21c ], %o1        ; o1 = largebitmapsize
	;; o0 is padByteLen
	;; o1 is largebitmapsize	
0x7047c4 <gc_large_alloc+28>:   add  %o0, %o1, %o0		; numerator (o0 + o1)
0x7047c8 <gc_large_alloc+32>:   call  0x8eedcc <.div>		; denonminator o1, result o0
0x7047cc <gc_large_alloc+36>:   add  %o0, -1, %o0		; delay slot:	numerator--
	;; o0 is chunksNeeded

	;; assert(byteLen >= 1024);
	
0x7047d0 <gc_large_alloc+40>:   cmp  %i1, 0x3ff
0x7047d4 <gc_large_alloc+44>:   bg  0x7047fc <gc_large_alloc+84>
0x7047d8 <gc_large_alloc+48>:   mov  %o0, %l1			; delay slot -- l1 is chunksNeeded
0x7047dc <gc_large_alloc+52>:   sethi  %hi(0x8dd800), %o0
0x7047e0 <gc_large_alloc+56>:   sethi  %hi(0x8dd800), %o1
0x7047e4 <gc_large_alloc+60>:   sethi  %hi(0x8dd800), %o3
0x7047e8 <gc_large_alloc+64>:   or  %o0, 0x2c8, %o0
0x7047ec <gc_large_alloc+68>:   or  %o1, 0x2e8, %o1
0x7047f0 <gc_large_alloc+72>:   or  %o3, 0x2f8, %o3
0x7047f4 <gc_large_alloc+76>:   call  0x70d570 <__eprintf>
0x7047f8 <gc_large_alloc+80>:   mov  0x22, %o2

	;; bitmapPos = AllocBitmapRange(allocMap,chunksNeeded);
	;; l1 is chunksNeeded
0x7047fc <gc_large_alloc+84>:   sethi  %hi(0x959c00), %o1	; setup for read from data segment
0x704800 <gc_large_alloc+88>:   ld  [ %o1 + 0x220 ], %o0        ; o0 = allocMap
0x704804 <gc_large_alloc+92>:   call  0x6f63e0 <AllocBitmapRange>
0x704808 <gc_large_alloc+96>:   mov  %l1, %o1			; delay slot:	o1 = chunksNeeded
	;; o0 is bitmapPos

	;;   if (bitmapPos < 0)
	;;     return NULL; /* allocation failed */
	
0x70480c <gc_large_alloc+100>:  orcc  %o0, 0, %o1		; compare (o0, 0)
0x704810 <gc_large_alloc+104>:  bl  0x704874 <gc_large_alloc+204>; if less, RETURN_NULL

	;;   region = largeSpace->bottom + (largebitmapsize * bitmapPos) / (sizeof (val_t));

0x704814 <gc_large_alloc+108>:  sethi  %hi(0x959c00), %o0	; setup for read from data segment
0x704818 <gc_large_alloc+112>:  ld  [ %o0 + 0x228 ], %l0        ; l0 = largeSpace

	;; compute o0 = largebitmapsize * bitmapPos
0x70481c <gc_large_alloc+116>:  call  0x8ef084 <.umul>
0x704820 <gc_large_alloc+120>:  ld  [ %i0 + 0x21c ], %o0	; delay slot: o0 = largebitmapsize
	
0x704824 <gc_large_alloc+124>:  ld  [ %l0 + 8 ], %o2		; o2 = largeSpace -> bottom
	
0x704828 <gc_large_alloc+128>:  and  %o0, -4, %o0		; ?? clear low 2 bits of o0
0x70482c <gc_large_alloc+132>:  add  %o2, %o0, %o2		; o2 is region

	;;   end = region + (chunksNeeded * largebitmapsize) / (sizeof (val_t));

0x704830 <gc_large_alloc+136>:  ld  [ %i0 + 0x21c ], %o1	; o1 = largebitmapsize

	;; *** SEGV ***; o2 = 0x3011f800; fp = 0x10101f10; (fp-20) = 0x10101efc
0x704834 <gc_large_alloc+140>:  st  %o2, [ %fp + -20 ]		; save region to frame

	;; compute o0 = chunksNeeded * largebitmapsize
0x704838 <gc_large_alloc+144>:  call  0x8ef084 <.umul>
0x70483c <gc_large_alloc+148>:  mov  %l1, %o0			; delay slot o0 = chunksNeeded
	
0x704840 <gc_large_alloc+152>:  ld  [ %fp + -20 ], %l0		; l0 = region (from frame)
0x704844 <gc_large_alloc+156>:  and  %o0, -4, %o0		; ?? clear low 2 bits of o0
0x704848 <gc_large_alloc+160>:  add  %l0, %o0, %l0		; l0 is end

	;;   AlignMemoryPointer(&region, align);

0x70484c <gc_large_alloc+164>:  mov  %i2, %o1			; o1 = align
0x704850 <gc_large_alloc+168>:  call  0x6f007c <AlignMemoryPointer>
0x704854 <gc_large_alloc+172>:  add  %fp, -20, %o0		; delay slot:	 o0 = &region

	;;   PadHeapArea(region + byteLen / sizeof(val_t), end);

	;; compute o0 = region + byteLen / sizeof(val_t)
0x704858 <gc_large_alloc+176>:  ld  [ %fp + -20 ], %o1		; o1 = region
0x70485c <gc_large_alloc+180>:  and  %i1, -4, %o0		; ?? o0 = byteLen, low 2 bits clear
0x704860 <gc_large_alloc+184>:  add  %o1, %o0, %o0
0x704864 <gc_large_alloc+188>:  call  0x6f85e4 <PadHeapArea>
0x704868 <gc_large_alloc+192>:  mov  %l0, %o1			; delay slot:	 o1 = end

	;;   return region;
0x70486c <gc_large_alloc+196>:  b  0x704878 <gc_large_alloc+208>
0x704870 <gc_large_alloc+200>:  ld  [ %fp + -20 ], %i0		; delay slot: i0 = region

RETURN_NULL:
0x704874 <gc_large_alloc+204>:  clr  %i0			; i0 = 0
	
0x704878 <gc_large_alloc+208>:  ret 
0x70487c <gc_large_alloc+212>:  restore				; delay slot:	 restore %fp %sp
