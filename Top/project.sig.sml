(*
	Accrete a project description by parsing and elaborating a
	collection of inter-dependent project description files.

	When Bootstrap is false, each non-basis project description
	file includes the compiled basis library project description
	file and each source unit or interface opens additional units
	that define the standard top-level environment.
*)
signature PROJECT =
sig

    val Bootstrap : bool ref

    type file = string

    (*
	Invariant:
	If d : desc,
	then |- (finish d) ok.

	In particular, add_include raises an exception if the included
	file is broken.
    *)
    type desc

    val empty : {linking:bool} -> desc

    val add_include : desc * file -> desc

    val finish : desc -> IntSyn.desc

    val print_ents : TextIO.outstream * ExtSyn.ents -> unit

end
