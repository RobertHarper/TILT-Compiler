structure Annotations : ANNOTATIONS =
struct

   type annotation = exn
   type annotations = annotation list

   exception COMMENT of string

   fun toString(COMMENT s) = s
     | toString e          = "<"^exnName e^">"

   (*
    * Look ma, a real use of generative exceptions!
    *)
   fun 'a new() =
   let exception Annotation of 'a
       fun get [] = NONE
         | get (Annotation x::_) = SOME x
         | get (_::l) = get l
       fun put(x,[]) = [Annotation x]
         | put(x,Annotation _::l) = Annotation x::l
         | put(x,y::l) = y::put(x,l)
       fun rmv [] = []
         | rmv (Annotation _::l) = rmv l
         | rmv (x::l) = x::rmv l
   in  { get=get, put=put, rmv=rmv }
   end

   fun get f []     = NONE
     | get f (x::l) = case f x of NONE => get f l | x => x  

   fun rmv f [] = []
     | rmv f (x::l) = if f x then rmv f l else x::rmv f l

   fun put(x,l) = x::l 

end

