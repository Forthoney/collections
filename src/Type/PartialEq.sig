signature PARTIAL_EQ =
sig
  type t
  val eq : t * t -> bool
  val ne : t * t -> bool
end
