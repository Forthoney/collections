functor Bogus_Option(type u) : BOGUS =
struct
  type t = u option
  val bogus = (NONE : u option)
  val isBogus = not o Option.isSome
end

functor Bogus_NonnegInt(I : INTEGER) : BOGUS =
struct
  type t = I.int
  val bogus = I.fromInt (~1)
  fun isBogus i = i = bogus
end
