(*$import Prelude DELAY *)

structure Delay :> DELAY =
struct
    datatype 'a result =
	Delayed of unit -> 'a
      | Value of 'a
      | Exn of exn

    type 'a value = 'a result ref

    (* delay : (unit -> a) -> 'a value *)
    fun delay f = ref (Delayed f)

    (* force : 'a value -> 'a *)
    fun force r = (case !r
		     of Delayed f => (r := (Value (f()) handle e => Exn e); force r)
		      | Value v => v
		      | Exn e => raise e)
end
