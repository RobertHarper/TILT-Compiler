#ifndef _register_h
#define _register_h


#define CFIRSTARG_REG    16
#define CSECONDARG_REG   17
#define CTHIRDARG_REG    18
#define CFOURTHARG_REG   19
#define CFIFTHARG_REG    20
#define CSIXTHARG_REG    21

#define FTMP_REG         30
#define ASMTMP_REG       28
#define ASMTMP2_REG      25
#define ASMTMP2_SYMREG   $25
#define ZERO_REG         31
#define EXNARG_REG       26
#define RA_REG           26
#define SP_REG           30
#define SP_DISP          (8 * SP_REG)
#define RA_DISP          (8 * RA_REG)
#define ALLOCSIZE_REG    ASMTMP_REG

#define EXNPTR_REG        15
#define EXNPTR_SYMREG        $15
#define ALLOCLIMIT_REG    14
#define ALLOCPTR_REG      13
#define ALLOCLIMIT_SYMREG $14
#define ALLOCPTR_SYMREG   $13
#define ALLOCLIMIT_DISP   (8 * ALLOCLIMIT_REG)
#define ALLOCPTR_DISP     (8 * ALLOCPTR_REG)
#define THREADPTR_REG     12
#define THREADPTR_SYMREG  $12



#define MLEXN_DIVZERO    1
#define MLEXN_OVERFLOW   2

#define little_endian

#endif
