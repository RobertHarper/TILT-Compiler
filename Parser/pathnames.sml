(*$import Prelude TopLevel PATHNAMES String *)

(* Copyright 1989 by AT&T Bell Laboratories *)

structure Pathnames : PATHNAMES =
  struct

  fun findChr (ch :string) ((i,s) :int * string) :int =
    let val len = String.size s
        fun find j =
          if j=len
          then 0
          else if ch = substring(s,j,1)
                 then j+1
                 else find (j+1)
    in if (size ch) = 0 then 0 else find i end;
  
  fun explodePath (path:string) :string list =
    let val slash = findChr "/" (0,path)
        val len = size path
    in
      if slash = 0
        then [path]
        else ((substring (path, 0, slash-1)) ::
              (explodePath (substring (path, slash, len - slash))))
    end;

  fun implodePath (pathlist :string list) :string =
    let fun merge (x,y) = if y = "" then x else (x ^ "/" ^ y) in
      foldr merge "" pathlist
    end;

  fun trim (path:string) :string = path
(*    let val parts = explodePath path
        val len = length parts
        val strip = if len<=2 then 0 else len-1
        val showParts' = nthtail (parts, strip)
        val showParts = if strip>0 then ("..."::showParts') else showParts'
    in
      implodePath showParts
    end
*)
end

(*
 * $Log$
# Revision 1.4  2000/09/12  18:57:06  swasey
# Changes for cutoff compilation
# 
# Revision 1.3  98/04/24  22:51:52  pscheng
# fixed imports
# 
# Revision 1.2  1998/01/21  20:40:40  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  14:12:32  pscheng
# added copy of SMLNJ parser files
# 
 *)
