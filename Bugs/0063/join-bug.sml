(*$import *)

signature LR_TABLE =
    sig
        datatype 'a pairlist = EMPTY
    end
functor JoinWithArg (structure ParserData: LR_TABLE) =
struct
    (* these fail, perhaps for different reasons *)
    (* structure LrTable = ParserData *)
    datatype 'a pairlist = datatype ParserData.pairlist
    
    (* these work, and should continue to work *)
    (* type 'a pairlist = 'a ParserData.pairlist *)
    (* structure LrTable = struct type 'a pairlist = 'a ParserData.pairlist end *)
end
