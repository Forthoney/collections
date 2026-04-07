functor PartialEq(
  type t
  val eq : t * t -> bool
) =
struct
  type t = t
  val eq = eq
  val ne = not o eq
end
