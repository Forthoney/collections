functor Ord(
  type t
  val compare : t * t -> order
) : ORD =
struct
  type t = t

  val compare = compare

  fun eq operands =
    compare operands = EQUAL

  fun ne operands =
    compare operands <> EQUAL
  
  fun lt operands =
    compare operands = LESS

  fun le operands =
    compare operands <> GREATER
      
  fun gt operands =
    compare operands = GREATER

  fun ge operands =
    compare operands <> LESS
end
