#ifndef _client_h
#define _client_h

#include "tag.h"

extern ptr_t client_entry;
extern int module_count;

extern val_t GCTABLE_BEGIN_VAL;
extern val_t GCTABLE_END_VAL;
extern val_t GLOBALS_BEGIN_VAL;
extern val_t GLOBALS_END_VAL;
extern val_t TRACE_GLOBALS_BEGIN_VAL;
extern val_t TRACE_GLOBALS_END_VAL;
extern val_t MUTABLE_TABLE_BEGIN_VAL;
extern val_t MUTABLE_TABLE_END_VAL;

#endif
