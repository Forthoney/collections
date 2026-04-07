signature PARTIAL_ORD =
sig
  val partialCompare : t * t -> order option

  val lt : t * t -> bool
  val le : t * t -> bool
  val gt : t * t -> bool
  val ge : t * t -> bool
end
