functor BinarySearchTreeBulk(T :
sig
  include BINARY_SEARCH_TREE
  val compare : key * key -> order
  val join : 'a t -> (key * 'a) -> 'a t -> 'a t
end) : BINARY_SEARCH_TREE_BULK =
struct
  open T

  fun splitLast (l, kv, r) =
    case expose r of
      NONE => (l, kv)
    | SOME r =>
      let val (t', kv') = splitLast r
      in
        (join l kv t', kv')
      end

  fun join2 l r =
    case expose l of
      NONE => r
    | SOME l =>
      let val (l', kv) = splitLast l
      in
        join l' kv r
      end

  fun split t k =
    case expose t of
      NONE => (empty, NONE, empty)
    | SOME (l, kv as (k', _), r) =>
      case compare (k, k') of
        EQUAL => (l, SOME kv, r)
      | LESS =>
        let val (leftL, b, rightL) = split l k
        in
          (leftL, b, join rightL kv r)
        end
      | GREATER =>
        let val (leftR, b, rightR) = split r k
        in
          (join l kv leftR, b, rightR)
        end

  fun union t1 t2 =
    case (expose t1, expose t2) of
      (NONE, _) => t2
    | (_, NONE) => t1
    | (_, SOME (l2, kv2 as (k2, _), r2)) =>
      let
        val (l1, _, r1) = split t1 k2
        val tL = union l1 l2
        val tR = union r1 r2
      in
        join tL kv2 tR
      end

  fun intersect t1 t2 =
    case (expose t1, expose t2) of
      (NONE, _) => empty
    | (_, NONE) => empty
    | (_, SOME (l2, kv2 as (k2, _), r2)) =>
      let
        val (l1, b, r1) = split t1 k2
        val tL = intersect l1 l2
        val tR = intersect r1 r2
      in
        case b of
          NONE => join2 tL tR
        | SOME kv => join tL kv tR
      end

  fun diff t1 t2 =
    case (expose t1, expose t2) of
      (NONE, _) => t1
    | (_, NONE) => t1
    | (_, SOME (l2, kv2 as (k2, _), r2)) =>
      let
        val (l1, _, r1) = split t1 k2
        val tL = diff l1 l2
        val tR = diff r1 r2
      in
        join2 tL tR
      end
end
