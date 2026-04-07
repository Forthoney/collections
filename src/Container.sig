signature CONTAINER =
sig
  type item
  type t

  val empty : t
  val isEmpty : t -> bool
  val insert : t -> item -> t
  val size : t -> int

  val toList : t -> item list
  val toArray : t -> item array
  val toVector : t -> item vector
end
