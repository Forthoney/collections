(* Binary search tree supporting bulk operations *)
signature BINARY_SEARCH_TREE_BULK =
sig
  include MAP_BULK

  val join : 'a t -> (key * 'a) -> 'a t -> 'a t
  val split : 'a t -> key -> 'a t * (key * 'a) option * 'a t
end
