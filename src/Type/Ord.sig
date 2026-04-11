signature ORD =
sig
  include EQ

  val compare : t * t -> order
  val lt : t * t -> bool
  val le : t * t -> bool
  val gt : t * t -> bool
  val ge : t * t -> bool
end
