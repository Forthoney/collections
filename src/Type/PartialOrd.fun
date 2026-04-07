functor PartialOrd(
  type t
  val partialCompare : t * t -> order option
) =
struct
  type t = t
  val partialCompare = partialCompare

  fun lt operands =
    case partialCompare operands of
      SOME LESS => true
    | _ => false

  fun le operands =
    case partialCompare operands of
      SOME LESS => true
    | SOME EQUAL => true
    | _ => false
      
  fun gt operands =
    case partialCompare operands of
      SOME GREATER => true
    | _ => false

  fun le operands =
    case partialCompare operands of
      SOME GREATER => true
    | SOME EQUAL => true
    | _ => false
end
