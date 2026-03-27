signature RESIZABLE_PARAM =
sig
  val loadLimit : real
  val growFactor : real
end

signature RESIZABLE =
sig
  val grow : {current : int, desired : int} -> int
end

functor Resizable(
  val loadLimit : real
  val growFactor : real
) : RESIZABLE =
struct
  fun grow {current, desired} =
    Real.round (Real.fromInt current * growFactor)
end
