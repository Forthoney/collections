(* Binary search tree supporting bulk operations *)
signature BINARY_SEARCH_TREE_BULK =
sig
  include BINARY_SEARCH_TREE

  val join : 'a t -> (key * 'a) -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val split : 'a t -> key -> 'a t * (key * 'a) option * 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
end
