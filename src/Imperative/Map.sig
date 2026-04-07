signature IMPERATIVE_MAP =
sig
  type key

  type 'a t

  val empty : unit -> 'a t
  val insert : 'a t -> (key * 'a) -> unit
  val lookup : 'a t -> key -> 'a option
  val remove : 'a t -> key -> 'a option

  val size : 'a t -> int
  val isEmpty : 'a t -> bool
end
