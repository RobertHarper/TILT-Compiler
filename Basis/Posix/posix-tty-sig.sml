(*$import Prelude SysWord POSIX_FLAGS General *)
(* posix-tty-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for POSIX 1003.1 operations on terminal devices
 *
 *)

signature POSIX_TTY =
  sig

    eqtype pid       (* process ID *)
    eqtype file_desc (* file descriptor *)
    
    structure V :
      sig
        val eof   : int
        val eol   : int
        val erase : int
        val intr  : int
        val kill  : int
        val min   : int
        val quit  : int
        val susp  : int
        val time  : int
        val start : int
        val stop  : int

        val nccs : int

        type cc

        val cc     : (int * char) list -> cc
        val update : cc * (int * char) list -> cc
        val sub    : cc * int -> char
      end
  
    structure I :
      sig
        include POSIX_FLAGS
        
        val brkint : flags
        val icrnl  : flags
        val ignbrk : flags
        val igncr  : flags
        val ignpar : flags
        val inlcr  : flags
        val inpck  : flags
        val istrip : flags
        val ixoff  : flags
        val ixon   : flags
        val parmrk : flags
      end

    structure O :
      sig
        include POSIX_FLAGS
        
        val opost : flags
      end

    structure C :
      sig
        include POSIX_FLAGS
        
        val clocal : flags
        val cread  : flags
        val cs5    : flags
        val cs6    : flags
        val cs7    : flags
        val cs8    : flags
        val csize  : flags
        val cstopb : flags
        val hupcl  : flags
        val parenb : flags
        val parodd : flags
      end

    structure L :
      sig
        include POSIX_FLAGS
        
        val echo   : flags
        val echoe  : flags
        val echok  : flags
        val echonl : flags
        val icanon : flags
        val iexten : flags
        val isig   : flags
        val noflsh : flags
        val tostop : flags
      end


    eqtype speed
    val compareSpeed : speed * speed -> General.order
    val speedToWord : speed -> SysWord.word
    val wordToSpeed : SysWord.word -> speed
    val b0     : speed
    val b50    : speed
    val b75    : speed
    val b110   : speed
    val b134   : speed
    val b150   : speed
    val b200   : speed
    val b300   : speed
    val b600   : speed
    val b1200  : speed
    val b1800  : speed
    val b2400  : speed
    val b4800  : speed
    val b9600  : speed
    val b19200 : speed
    val b38400 : speed
    
    type termios
    
    val termios : { iflag : I.flags,
                    oflag : O.flags,
                    cflag : C.flags,
                    lflag : L.flags,
                    cc : V.cc,
                    ispeed : speed,
                    ospeed : speed } -> termios

    val fieldsOf : termios -> { iflag : I.flags,
                                oflag : O.flags,
                                cflag : C.flags,
                                lflag : L.flags,
                                cc : V.cc,
                                ispeed : speed,
                                ospeed : speed }

    val getiflag  : termios -> I.flags
    val getoflag  : termios -> O.flags
    val getcflag  : termios -> C.flags
    val getlflag  : termios -> L.flags
    val getcc     : termios -> V.cc

    structure CF :
      sig
	  val getospeed : termios -> speed
	  val setospeed : termios * speed -> termios
	  val getispeed : termios -> speed
	  val setispeed : termios * speed -> termios
      end
    
    structure TC :
      sig
        eqtype set_action

        val sanow   : set_action
        val sadrain : set_action
        val saflush : set_action

        eqtype flow_action

        val ooff : flow_action
        val oon  : flow_action
        val ioff : flow_action
        val ion  : flow_action

        eqtype queue_sel

        val iflush  : queue_sel
        val oflush  : queue_sel
        val ioflush : queue_sel
	val getattr : file_desc -> termios
	val setattr : file_desc * set_action * termios -> unit
	val sendbreak : file_desc * int -> unit
	val drain : file_desc -> unit
	val flush : file_desc * queue_sel -> unit
	val flow : file_desc * flow_action -> unit
      end

    val getpgrp : file_desc -> pid
    val setpgrp : file_desc * pid -> unit

  end (* signature POSIX_TTY *)

(*
 * $Log$
# Revision 1.4  2001/12/13  16:31:30  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/11/27  22:36:40  swasey
# *** empty log message ***
# 
 * Revision 1.2  2000/09/12 18:54:42  swasey
 * Changes for cutoff compilation
 *
# Revision 1.1  98/03/09  19:53:41  pscheng
# added basis
# 
 * Revision 1.2  1997/06/07  15:27:42  jhr
 *   SML'97 Basis Library changes (phase 3; Posix changes)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
