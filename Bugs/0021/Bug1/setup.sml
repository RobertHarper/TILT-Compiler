(*$import TopLevel *)

signature SIG =
sig
    type b
    type 'a c
end

functor F(type a
	  val f : a * a -> order) :> SIG where type b = a =
struct
    type b = a
    datatype 'a c = DUMMY
end
