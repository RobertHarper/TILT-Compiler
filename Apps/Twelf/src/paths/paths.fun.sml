(* Paths, Occurrences, and Error Locations *)
(* Author: Frank Pfenning *)

functor Paths () :> PATHS =
struct

  type pos = int			(* characters, starting at 0 *)
  datatype region = Reg of pos * pos	(* r ::= (i,j) is interval [i,j) *)
  datatype location = Loc of string * region (* loc ::= (filename, region) *)

  local
    (* !linePosList is a list of starting character positions for each input line *)
    (* used to convert character positions into line.column format *)
    (* maintained with state *)
    val linePosList = ref nil : pos list ref
  in
    fun resetLines () = linePosList := nil
    fun newLine (i) = linePosList := i::(!linePosList)
    (* posToLineCol (i) = (line,column) for character position i *)
    fun posToLineCol (i) =
        let fun ptlc (j::js) = if i >= j then (List.length js, i-j)
			       else ptlc js
	      (* first line should start at 0 *)
	      (* nil means first "line" was not terminated by <newline> *)
	      | ptlc (nil) = (0, i)
	in
	  ptlc (!linePosList)
	end
  end

  (* join (r1, r2) = r
     where r is the  smallest region containing both r1 and r2
  *)
  fun join (Reg (i1, j1), Reg (i2, j2)) = Reg (Int.min (i1, i2), Int.max (j1, j2))

  (* The right endpoint of the interval counts IN RANGE *)
  fun posInRegion (k, Reg (i,j)) = i <= k andalso k <= j

  fun lineColToString (line,col) =
      Int.toString (line+1) ^ "." ^ Int.toString (col+1)

  (* toString r = "line1.col1-line2.col2", a format parsable by Emacs *)
  fun toString (Reg (i,j)) =
        lineColToString (posToLineCol i) ^ "-"
	^ lineColToString (posToLineCol j)

  (* wrap (r, msg) = msg' which contains region *)
  fun wrap (r, msg) = (toString r ^ " " ^ "Error: \n" ^ msg)

  (* wrapLoc ((filename, r), msg) = msg' which contains region and filename
     This should be used for locations retrieved from origins, where
     the region is given in character positions, rather than lines and columns
  *)
  fun wrapLoc (Loc (filename, Reg (i,j)), msg) =
        filename ^ ":" ^ Int.toString (i+1) ^ "-" ^ Int.toString (j+1)
	^ " " ^ "Error: \n" ^ msg

  (* Paths, occurrences and occurrence trees only work well for normal forms *)
  (* In the general case, regions only approximate true source location *)

  (* Follow path through a term to obtain subterm *)

  datatype Path =
     Label of Path			(* [x:#] U or {x:#} V *)
   | Body of Path			(* [x:V] # or {x:V} # *)
   | Head				(* # @ S, term in normal form *)
   | Arg of int * Path			(* C @ S1; ...; #; ...; Sn; Nil *)
   | Here				(* #, covers Uni, EVar, Redex(?) *)

  (* Occurrences: paths in reverse order *)
  (* could also be: type occ = path -> path *)
  datatype occ =
      top
    | label of occ
    | body of occ
    | head of occ
    | arg of int * occ

  (* Occurrence trees for expressions and spines *)
  (* Maps occurrences to regions *)
  (* Simple-minded implementation *)
  (* A region in an intermediate node encloses the whole expression *)
  datatype occExp =			(* occurrences in expressions *)
      leaf of region			(* _ or identifier *)
    | bind of region * occExp option * occExp (* [x:vOpt] u or {x:vOpt} v' *)
    | root of region * occExp * int * occSpine (* h @ s, # of implicit arguments *)
  and occSpine =			(* occurrences in spines *)
      app of occExp * occSpine		(* u;s *)
    | nils				(* nil *)

  (* occToPath (occ, p) = p'(p) and occ corresponds to p' *)
  fun occToPath (top, path) = path
    | occToPath (label(occ), path) = occToPath (occ, Label(path))
    | occToPath (body(occ), path) = occToPath (occ, Body(path))
    | occToPath (head(occ), path) =
      (* path = Here by invariant *)
        occToPath (occ, Head)
    | occToPath (arg(n,occ), path) = occToPath (occ, Arg(n,path))

  datatype occConDec =			(* occurrence tree for constant declarations *)
      dec of region * int * occExp	(* (r, #implicit, v) in c : V *)
    | def of region * int * occExp * occExp option
					(* (r, #implicit, u, v) in c : V = U *)

  (* val posToPath : occExp -> pos -> Path *)
  (* posToPath (u, k) = p
     where p is the path to the innermost expression in u enclosing position i.

     This includes the position immediately at the end of a region [i,j).
     For example, in "f (g x) y",
     0,1 => "f"
     2   => "(g x)"
     3,4 => "g"
     5,6 => "x"
     8,9 => "y"
  *)
  fun posToPath u k =
      let
          (* local functions refer to k but not u *)
	  fun inside (leaf r) = posInRegion (k, r)
	    | inside (bind (r, _, _)) = posInRegion (k, r)
	    | inside (root (r, _, _, _)) = posInRegion (k, r)

	  fun toPath (leaf (Reg (i,j))) = Here (* check? mark? *)
	    | toPath (bind (Reg (i,j), NONE, u)) =
              if inside u then Body (toPath u)
	      else Here
	    | toPath (bind (Reg (i,j), SOME(u1), u2)) =
	      if inside u1 then Label (toPath u1)
	      else if inside u2 then Body (toPath u2)
		   else Here
	    | toPath (root (Reg (i,j), h, imp, s)) =
	      if inside h then Head
	      else (case toPathSpine (s, 1)
		      of NONE => Here
		       | SOME(n, path) => Arg (n+imp, path))
	  (* in some situations, whitespace after subexpressions *)
          (* might give a larger term than anticipated *)
	  and toPathSpine (nils, n) = NONE
	    | toPathSpine (app(u,s), n) =
	      if inside u then SOME(n, toPath u)
	      else toPathSpine (s, n+1)
      in
	toPath u
      end

  (* toRegion (u) = r, the region associated with the whole occurrence tree u *)
  fun toRegion (leaf r) = r
    | toRegion (bind (r, _, _)) = r
    | toRegion (root (r, _, _, _)) = r

  (* toRegionSpine (s, r) = r', the join of all regions in s and r *)
  fun toRegionSpine (nils, r) = r
    | toRegionSpine (app (u, s), r) =
        join (toRegion u, toRegionSpine (s, r))	(* order? *)

  (* pathToRegion (u, p) = r,
     where r is the region identified by path p in occurrence tree u
  *)
  fun pathToRegion (u, Here) = toRegion u
    | pathToRegion (bind (r, NONE, u), Label(path)) =
      (* addressing implicit type label returns region of binder and its scope *)
      r
    | pathToRegion (bind (r, SOME(u1), u2), Label(path)) =
        pathToRegion (u1, path)
    | pathToRegion (bind (r, _, u), Body(path)) =
	pathToRegion (u, path)
    | pathToRegion (root (r, h, imp, s), Head) = toRegion h
    | pathToRegion (root (r, h, imp, s), Arg (n, path)) =
      if n <= imp
	then (* addressing implicit argument returns region of head *)
	     toRegion h
      else pathToRegionSpine (s, n-imp, path)
    (* other combinations should be impossible *)
  and pathToRegionSpine (app (u, s), 1, path) =
        pathToRegion (u, path)
    | pathToRegionSpine (app (u, s), n, path) =
	pathToRegionSpine (s, n-1, path)
    (* anything else should be impossible *)

  (* occToRegionExp u occ = r,
     where r is the closest region including occ in occurrence tree u
  *)
  fun occToRegionExp u occ = pathToRegion (u, occToPath (occ, Here))

  fun skipImplicit (r, 0, path) = path
    | skipImplicit (r, n, Body(path)) =
        skipImplicit (r, n-1, path)
    | skipImplicit (r, n, Label(path)) =
	(* implicit argument: approximate as best possible *)
	Here
    | skipImplicit (r, n, Here) =
	(* addressing body including implicit arguments: approximate by body *)
	Here
    (* anything else should be impossible *)

  (* occToRegionDec d occ = r
     where r is the closest region in v including occ for declaration c : V
  *)
  fun occToRegionDec (dec (r, n, v)) occ =
      pathToRegion (v, skipImplicit (r, n, occToPath (occ, Here)))

  (* occToRegionDef1 d occ = r
     where r is the closest region in u including occ for declaration c : V = U
  *)
  fun occToRegionDef1 (def (r, n, u, vOpt)) occ =
      pathToRegion (u, skipImplicit (r, n, occToPath (occ, Here)))

  (* occToRegionDef2 d occ = r
     where r is the closest region in V including occ for declaration c : V = U
  *)
  fun occToRegionDef2 (def (r, n, u, NONE)) occ = r
    | occToRegionDef2 (def (r, n, u, SOME(v))) occ =
      pathToRegion (v, skipImplicit (r, n, occToPath (occ, Here)))

end;  (* functor Paths *)
