#ifndef _client_h
#define _client_h

#include "tag.h"

extern int link_modulecount;
extern val_t link_gctable;
extern val_t link_globalstart;
extern val_t link_globalend;
extern val_t link_traceglobalstart;
extern val_t link_traceglobalend;

/* Access these like val = GetGlobal(&label) */
extern val_t ml__PLUSUTiltExn__INT__r__INT;
extern val_t link_LINKUNIT_DOTmain;

#endif
