(*$import *)

signature PARSER_DATA =
sig
    datatype 'a token = TOKEN
end

functor JoinWithArg (structure ParserData : PARSER_DATA) =
struct
end
