#ifndef _register_h
#define _register_h


#define CFIRSTARG    16
#define CSECONDARG   17
#define CTHIRDARG    18
#define CFOURTHARG   19
#define CFIFTHARG    20
#define CSIXTHARG    21
#define RESULT       16

#define FTMP         30
#define ASMTMP       28
#define ASMTMP2      25
#define ASMTMP2_REG  $25
#define ZERO         31
#define EXNARG       26
#define EXNARG_REG   $26
#define RA           26
#define SP           30
#define SP_DISP      (8 * SP)
#define EXNARG_DISP  (8 * EXNARG)
#define RA_DISP      (8 * RA)
#define ALLOCSIZE    ASMTMP

#define EXNPTR          15
#define EXNPTR_REG      $15
#define ALLOCLIMIT      14
#define ALLOCPTR        13
#define ALLOCLIMIT_REG  $14
#define ALLOCPTR_REG    $13
#define THREADPTR       12
#define THREADPTR_REG   $12
#define ALLOCLIMIT_DISP (8 * ALLOCLIMIT)
#define ALLOCPTR_DISP   (8 * ALLOCPTR)


#define MLEXN_DIVZERO    1
#define MLEXN_OVERFLOW   2

#define little_endian

#endif
