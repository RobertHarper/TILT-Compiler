(*$import Prelude COMMAND_LINE CommandLineHelp *)

structure CommandLine :> COMMAND_LINE =
struct

    fun name () = Ccall(commandline_name,())
    fun arguments () = Ccall(commandline_arguments,())

end
