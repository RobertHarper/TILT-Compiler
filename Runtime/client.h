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

/* Locations of important exceptions.  Changes to name.sml, toil.sml, or tonil.sml 
 * may affect these.  */

/* This should be set when Tonil.flatten_modules is true.
 * Otherwise it should be unset.
 * */
#define FLATTEN_MODULES
#undef FLATTEN_MODULES
#ifdef FLATTEN_MODULES
extern val_t  ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTDiv__0_DOTmk__i__INT;
extern val_t  ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTDiv__0_DOTstamp__i__INT;
extern val_t  ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTOverflow__0_DOTmk__i__INT;
extern val_t  ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTOverflow__0_DOTstamp__i__INT;
extern val_t  ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTTiltExn__2_DOTSysErr__0_DOTstamp__i__INT;
extern val_t  ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTTiltExn__2_DOTLibFail__0_DOTstamp__i__INT;
#else
extern val_t ml__PLUSUTiltExn__INT__r__INT;
#endif

extern val_t link_LINKUNIT_DOTmain;

#endif
