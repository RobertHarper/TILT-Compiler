(*$import Prelude *)

(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

signature GRAPHUTIL =
sig

  val topsort : int -> (int * int) list -> int list
  (* Given the number of vertices (vertices are numbered 0..n-1),   *)
  (* and a list of directed edges forming an acyclic graph, return  *)
  (* a topologically sorted list of vertices.                       *)
  (*                                                                *)
  (* Example:                                                       *)
  (*   topsort 5 [(0,1),(0,2),(3,2),(1,4),(2,4)] =                  *)
  (*     [3,0,1,2,4]                                                *)
  (*                                                                *)
  (* Warning:                                                       *)
  (*   topsort will not detect if your graph contains cycles.       *)
  (*   If there are cycles, the list will topologically sorted      *)
  (*   with respect to those vertices that are not involved in      *)
  (*   cycles, but the vertices of the cycles may appear in         *)
  (*   arbitrary order.                                             *)


  val scc : int -> (int * int) list -> int list list
  (* Given the number of vertices (vertices are numbered 0..n-1),   *)
  (* and a list of directed edges, compute the strongly connected   *)
  (* components.  Return a list of components, where each component *)
  (* is a list of vertices.  Components are ordered so that there   *)
  (* are no edges from a later component to an earlier component.   *)
  (*                                                                *)
  (* Example:                                                       *)
  (*   scc 5 [(0,1),(1,0),(1,2),(2,3),(3,4),(4,3)] =                *)
  (*     [[1,0], [2], [4,3]]                                        *)


end
