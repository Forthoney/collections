signature BINARY_SEARCH_TREE =
sig
  type key
  type 'a t

  val empty : 'a t
  val singleton : (key * 'a) -> 'a t
  val expose : 'a t -> ('a t * (key * 'a) * 'a t) option

  val insert : 'a t -> (key * 'a) -> 'a t
  val lookup : 'a t -> key -> 'a option
  val remove : 'a t -> key -> 'a option
  val member : 'a t -> key -> bool

  val size : 'a t -> int
  val isEmpty : 'a t -> bool
end
