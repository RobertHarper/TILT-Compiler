(*$import Prelude *)
(*
signature TOKEN =
sig
    datatype term = T of int
    datatype 'a token = TOKEN of term * 'a
end

structure Token : TOKEN  =
struct
    datatype term = T of int
    datatype 'a token = TOKEN of term * 'a
    fun works (t,TOKEN (t',_)) = t=t'
end

fun fail (t, Token.TOKEN (t',_)) = t=t'
*)

signature TOKEN =
sig
    datatype term = T of int
    datatype token = TOKEN of term
end

structure Token : TOKEN  =
struct
    datatype term = T of int
    datatype token = TOKEN of term
    fun works (t,TOKEN t') = t=t'
end
(*
fun works_two (t : Token.term, t') = t=t'
fun works_three (t : Token.token, t') = t=t'
*)
fun fails (t, Token.TOKEN t') = t=t'
