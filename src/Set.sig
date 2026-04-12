signature SET =
sig
  type item
  type t

  val empty : t
  val singleton : item -> t
  val insert : t -> item -> t
  val member : t -> item -> bool
  val remove : t -> item -> (bool * t)

  val size : t -> int
  val isEmpty : t -> bool

  val union : t -> t -> t
  val intersect : t -> t -> t
  val diff : t -> t -> t
end
