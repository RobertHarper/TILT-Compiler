(*$import TopLevel TraceTable Linkrtl *)

(* =========================================================================
 * LittleEndianTraceTable.sml
 * ========================================================================= *)

structure LittleEndianTraceTable = Tracetable(val little_endian = true
					      structure Rtl     = Linkrtl.Rtl)

