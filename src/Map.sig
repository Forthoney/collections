signature MAP =
sig
  type key

  type 'a t

  val empty : 'a t 
  val insert : 'a t -> (key * 'a) -> 'a t
  val lookup : 'a t -> key -> 'a option
  val remove : 'a t -> key -> 'a option

  val size : 'a t -> int
  val isEmpty : 'a t -> bool

  val toList : 'a t -> (key * 'a) list
end
