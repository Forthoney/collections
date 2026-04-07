signature IMPERATIVE_SET =
sig
  type t
  type item

  val empty : unit -> t
  val new : int -> t
  val size : t -> int
  val isEmpty : t -> bool
  val member : t -> item -> bool
  val insert : t -> item -> unit
  val delete : t -> item -> unit

  (* val union : t -> t -> t *)
  (* val inter : t -> t -> t *)
  (* val diff : t -> t -> t *)
  (* val partition : (item -> bool) -> t -> (t * t) *)

  val app : (item -> unit) -> t -> unit
  val reduce : (item * 'b -> 'b) -> 'b -> t -> 'b
  val find : (item -> bool) -> t -> item option
  val filter : (item -> bool) -> t -> unit

  val toList : t -> item list
  val toArray : t -> item array
  val toVector : t -> item vector
end
