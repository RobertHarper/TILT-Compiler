signature NODE_PRIORITY_QUEUE =
sig

   type node_priority_queue

   exception EmptyPriorityQueue

   val create         : (Graph.node_id * Graph.node_id -> bool) -> 
                           node_priority_queue 
   val fromGraph      : (Graph.node_id * Graph.node_id -> bool) -> 
                          ('n,'e,'g) Graph.graph -> node_priority_queue
   val isEmpty        : node_priority_queue -> bool
   val clear          : node_priority_queue -> unit
   val min            : node_priority_queue -> Graph.node_id 
   val deleteMin      : node_priority_queue -> Graph.node_id
   val decreaseWeight : node_priority_queue * Graph.node_id -> unit
   val insert         : node_priority_queue * Graph.node_id -> unit
   val toList         : node_priority_queue -> Graph.node_id list
end

functor NodePriorityQueueFn(A : ARRAY_SIG) : NODE_PRIORITY_QUEUE =
struct
   structure G = Graph

   exception EmptyPriorityQueue

   datatype node_priority_queue = 
       Q of { <    : G.node_id * G.node_id -> bool,
              heap : G.node_id A.array, 
              pos  : int A.array, 
              size : int ref
            }
   fun create less = Q { <    = less, 
                         heap = A.array(13,0),
                         pos  = A.array(13,0),
                         size = ref 0
                       }
  
   fun isEmpty (Q { size = ref 0, ... }) = true
     | isEmpty _ = false

   fun clear (Q { size, ... }) = size := 0

   fun min   (Q { size = ref 0, ... }) = raise EmptyPriorityQueue
     | min   (Q { heap, ... }) = A.sub(heap, 0)

   fun decreaseWeight(Q { size, heap, pos, <}, x) =
   let fun siftup 0 = 0
         | siftup i =
           let val j = (i-1) div 2
               val y = A.sub(heap,j)
           in  if x < y then (A.update(heap,i,y); A.update(pos,y,i); siftup j)
               else i
           end 
       val x_pos = siftup(A.sub(pos,x))
   in  
       A.update(heap,x_pos,x); A.update(pos,x,x_pos)
   end

   fun insert(Q { size, heap, pos, < }, x) =
   let val N = !size
       fun siftup 0 = 0
         | siftup i =
           let val j = (i-1) div 2
               val y = A.sub(heap,j)
           in  if x < y then (A.update(heap,i,y); A.update(pos,y,i); siftup j)
               else i
           end 
       val x_pos = siftup N
   in  
       A.update(heap,x_pos,x); A.update(pos,x,x_pos);
       size := N + 1
   end

   fun deleteMin(Q { size = ref 0, heap, pos, <}) = raise EmptyPriorityQueue
     | deleteMin(Q { size, heap, pos, <}) =
   let val N = !size - 1
       fun siftdown (i,x) = 
       let val j = i*2 + 1
           val k = j + 1
       in  if j >= N then i
           else if k >= N then
              let val y = A.sub(heap,j)
              in  if y < x then go(i,x,j,y) else i 
              end
           else 
              let val y = A.sub(heap,j)
                  val z = A.sub(heap,k)
              in  if y < x andalso not(z < y) then go(i,x,j,y) 
                  else if z < x andalso not(y < z) then go(i,x,k,z)
                  else i
              end
       end
       and go(i,x,j,y) = (A.update(heap,i,y); A.update(pos,y,i); siftdown(j,x))
       val min   = A.sub(heap,0)
       val x     = A.sub(heap,N)
       val x_pos = siftdown(0, x)
   in  A.update(heap,x_pos,x); A.update(heap,x,x_pos); 
       size := N;
       min
   end

   fun toList (Q { heap, size, ... }) = 
       A.foldli (fn (i,x,l) => x::l) [] (heap,0,SOME(!size-1))

   fun fromGraph op< (G.GRAPH G) =
   let val N    = #order G ()
       val heap = A.array(N,0) 
       val pos  = A.array(N,0) 
       fun siftdown (i,x) = 
       let val j = i*2 + 1
           val k = j + 1
       in  if j >= N then A.update(heap,i,x)
           else if k >= N then
              let val y = A.sub(heap,j)
              in  if y < x then go(i,x,j,y) else A.update(heap,i,x)
              end
           else 
              let val y = A.sub(heap,j)
                  val z = A.sub(heap,k)
              in  if y < x andalso not(z < y) then go(i,x,j,y) 
                  else if z < x andalso not(y < z) then go(i,x,k,z)
                  else A.update(heap,i,x)
              end
       end
       and go(i,x,j,y) = (A.update(heap,i,y); siftdown(j,x))
 
       fun make_heap ~1 = ()
         | make_heap i = (siftdown(i,A.sub(heap,i)); make_heap(i-1))

       val _ = make_heap(N div 2)

       val _ = A.appi (fn (i,x) => A.update(pos,x,i)) (heap,0,NONE)

   in  Q { < = op <, heap = heap, pos = pos, size = ref N } 
   end
end

