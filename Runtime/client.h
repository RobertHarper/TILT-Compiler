#ifndef _client_h
#define _client_h

#include "tag.h"

extern ptr_t client_entry;

extern int ml_module_count;
extern val_t ml_GCTABLE_BEGIN_VAL;
extern val_t ml_GCTABLE_END_VAL;
extern val_t ml_GLOBALS_BEGIN_VAL;
extern val_t ml_GLOBALS_END_VAL;
extern val_t ml_TRACE_GLOBALS_BEGIN_VAL;
extern val_t ml_TRACE_GLOBALS_END_VAL;

/* Access these like val = GetGlobal(&label) */
extern val_t ml_Div_r_INT;
extern val_t ml_Overflow_r_INT;
extern val_t ml_TiltExn_STR_r_INT;
extern val_t ml_LINKUNIT_unit;

#endif
