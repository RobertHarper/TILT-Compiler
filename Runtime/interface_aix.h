#ifndef _register_h
#define _register_h


#define CFIRSTARG_REG    3
#define CSECONDARG_REG   4
#define CTHIRDARG_REG    5
#define CFOURTHARG_REG   6
#define CFIFTHARG_REG    7
#define CSIXTHARG_REG    8
#define ASMTMP_REG       31
#define ASMTMP2_REG       30

#define ALLOCPTR_REG     28
#define ALLOCLIMIT_REG   26
#define ALLOCSIZE_REG    ASMTMP_REG
#define GCRETADD_REG     ALLOCLIMIT_REG
#define EXNPTR_REG       27
#define EXNARG_REG       29



#define MLEXN_DIVZERO    1
#define MLEXN_OVERFLOW   2


#define big_endian

#endif
