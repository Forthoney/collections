signature MAP =
sig
  type key
  type 'a t

  val empty : 'a t
  val singleton : key * 'a -> 'a t
  val insert : 'a t -> (key * 'a) -> 'a t
  val member : 'a t -> key -> bool
  val remove : 'a t -> key -> ('a option * 'a t)
  val get : 'a t -> key -> 'a option

  val size : 'a t -> int
  val isEmpty : 'a t -> bool
end
