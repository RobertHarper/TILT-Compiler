#ifndef _register_h
#define _register_h

#define longsize         4

#define CFIRSTARG    8
#define CSECONDARG   9
#define CTHIRDARG    10
#define CFOURTHARG   11
#define CFIFTHARG    12
#define CSIXTHARG    13
#define RESULT       8
#define CFIRSTARG_REG %r8
#define CSECONDARG_REG %r9
#define CTHIRDARG_REG %r10
#define RESULT_REG    %r8

#define FTMP         31
#define ASMTMP       16
#define ASMTMP_REG    %r16
#define ASMTMP2      17
#define ASMTMP2_REG   %r17
#define ZERO         0
#define EXNARG       15
#define EXNARG_REG    %r15
#define RA	     15 
#define RA_REG       %r15
#define SP           14
#define SP_REG        %r14
#define EXNARG_DISP      (longsize * EXNARG)
#define SP_DISP          (longsize * SP)
#define RA_DISP          (longsize * RA)
#define ASMTMP_DISP      (longsize * ASMTMP)
#define ASMTMP2_DISP     (longsize * ASMTMP2)
#define ALLOCSIZE    ASMTMP

#define EXNPTR        1
#define EXNPTR_REG     %r1
#define ALLOCPTR      4
#define ALLOCPTR_REG   %r4
#define ALLOCLIMIT    5
#define ALLOCLIMIT_REG %r5
#define ALLOCLIMIT_DISP   (longsize * ALLOCLIMIT)
#define ALLOCPTR_DISP     (longsize * ALLOCPTR)
#define THREADPTR     2
#define THREADPTR_REG  %r2

#define MLEXN_DIVZERO    1
#define MLEXN_OVERFLOW   2

#undef  little_endian
#define big_endian

#endif
