functor RegMapFn(type value 
                 val join : value * value -> value
                 val meet : value * value -> value
                 val toString : value -> string
                 val ==   : value * value -> bool
                ) : REGISTER_MAP =
struct

   type reg = int
   type value = value
   type regmap = (reg * value) list

   val empty = [] 

   fun split ([],a,b)    = (a,b)
     | split (r::rs,a,b) = split(rs,r::b,a)

   fun mergeUniq(l as (u as (a,x))::us, l' as (v as (b,y))::vs) =
         if a = b then mergeUniq((a,join(x,y))::us,vs)
         else if Int.<(a,b) then u::mergeUniq(us,l')
         else v::mergeUniq(l,vs)
     | mergeUniq(l,[]) = l
     | mergeUniq([],l) = l

   fun sort [] = []
     | sort (l as [_]) = l
     | sort (l as [x as (a,u),y as (b,v)]) = 
          if Int.<(a,b) then l 
          else if a = b then [(a,join(u,v))] else [y,x]
     | sort l =
          let val (a,b) = split (l,[],[])
          in  mergeUniq(sort a, sort b)
          end

   fun union [] = []
     | union (r::rs) = mergeUniq(r,union rs)

   fun intersect(_,[]) = []
     | intersect([],_) = []
     | intersect(set as (r as (a,x))::rs,set' as (r' as (b,y))::rs') =
          if a = b then (a,meet(x,y))::intersect(rs,rs')
          else if a < b then intersect(rs,set')
          else intersect(set,rs')

   fun intersects []  = []
     | intersects [a] = a
     | intersects (a::b) = intersect(a,intersects b)
  
   fun eq([],[]) = true
     | eq((r,a)::rs,(r',b)::rs') = 
          (r : int) = r' andalso ==(a,b) andalso eq(rs,rs')
     | eq(_,_)   = false

   val == = eq

   fun isEmpty [] = true
     | isEmpty _  = false

   val app = List.app

   fun contains ([], r)    = false
     | contains ((r',_)::rs,r) = r' = r orelse (r > r' andalso contains(rs,r))

   fun exists (set, [])    = false
     | exists (set, r::rs) = contains(set,r) orelse exists(set,rs)

   fun insert([],r,x) = [(r,x)]
     | insert(set as (r' as (a,x))::rs,b,y) =
         if a = b then (a,join(x,y))::rs
         else if a < b then r'::insert(rs,b,y)
         else (b,y)::set

   fun remove ([],r) = []
     | remove (set as (r' as (a,_))::rs,r) =
         if a = r then rs
         else if a < r then r'::remove(rs,r)
         else set
     
   fun fromList l       = sort l
   fun fromSortedList l = l
   fun toList set       = set

   fun show set =
   let fun collect([],l) = l
         | collect((r,x)::rs,l) = 
              Int.toString r::"->"::toString x::collect'(rs,l)
       and collect'(rs,l) = 
           let val l = collect(rs,l)
           in  case l of [_] => l 
                       | l  => ","::l
           end
   in  String.concat("{"::collect(set,["}"]))
   end

   val toString = show
   val op + = mergeUniq
   val op * = intersect

end

