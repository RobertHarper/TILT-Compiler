(*$import *)
signature A = sig type t end
signature B = sig include A where type t = unit end
