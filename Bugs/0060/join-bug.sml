(*$import *)

signature TOKEN =
    sig
    end

signature PARSER_DATA =
   sig
 	type arg

	structure Token : TOKEN
    end

signature ARG_PARSER = 
    sig
        structure Token : TOKEN
    end

functor JoinWithArg (structure ParserData: PARSER_DATA) :> ARG_PARSER =
struct
    structure Token = ParserData.Token

    type arg = ParserData.arg
end
