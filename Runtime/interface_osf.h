#ifndef _register_h
#define _register_h


#define CFIRSTARG_REG    16
#define CSECONDARG_REG   17
#define CTHIRDARG_REG    18
#define CFOURTHARG_REG   19
#define CFIFTHARG_REG    20
#define CSIXTHARG_REG    21
#define ASMTMP_REG       28
#define ASMTMP2_REG      25
#define ZERO_REG         31
#define EXNARG_REG       26

#define ALLOCSIZE_REG    ASMTMP_REG

#define EXNPTR_REG       9
#define ALLOCLIMIT_REG   10
#define ALLOCPTR_REG     11
#define GCRETADD_REG     ALLOCLIMIT_REG


#define MLEXN_DIVZERO    1
#define MLEXN_OVERFLOW   2

#define little_endian

#endif
