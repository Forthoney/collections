signature SET =
sig
  include CONTAINER

  val new : int -> t
  val member : t -> item -> bool
  val delete : t -> item -> t

  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val partition : (item -> bool) -> t -> (t * t)

  val map : (item -> item) -> t -> t
  val mapPartial : (item -> item option) -> t -> t
  val app : (item -> unit) -> t -> unit
  val reduce : (item * 'b -> 'b) -> 'b -> t -> 'b
  val find : (item -> bool) -> t -> item option
  val filter : (item -> bool) -> t -> t
end
