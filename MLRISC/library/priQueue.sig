signature PRIORITY_QUEUE =
sig

   type 'a priority_queue

   exception EmptyPriorityQueue

   val create    : ('a * 'a -> bool) -> 'a priority_queue 
   val isEmpty   : 'a priority_queue -> bool
   val clear     : 'a priority_queue -> unit
   val min       : 'a priority_queue -> 'a
   val deleteMin : 'a priority_queue -> 'a
   val merge     : 'a priority_queue * 'a priority_queue -> 'a priority_queue
   val mergeInto : { src : 'a priority_queue, dst : 'a priority_queue } -> unit
   val insert    : 'a priority_queue -> 'a -> unit
   val fromList  : ('a * 'a -> bool) -> 'a list -> 'a priority_queue
   val toList    : 'a priority_queue -> 'a list
   val rerank    : 'a priority_queue -> unit

end
