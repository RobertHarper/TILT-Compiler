(*$import TextIO Time *)
signature MAKEDEP = sig
  
  type unitName = string

  datatype file = SML of {name:unitName, path:string, imports:unitName list, time:Time.time} 
  | INT of {name:unitName, path:string, includes:unitName list, time:Time.time} 
  | UO of {name:unitName, path:string, time:Time.time} 
  | UI of {name:unitName, path:string, time:Time.time} 

  type dependency = file * file list

  type project = unitName * file list * dependency list

  exception FileNotFound
  exception BadFile

  val openProject : string -> project
  val printFiles : project * TextIO.outstream -> unit
  val outputDependencies : project * TextIO.outstream -> unit
  val appendDepsToFile : project * string -> unit

  val mkDep : string -> unit  (* Generates the dependencies for the current directory and 
			       * appends them to a given Makefile. *)

end
