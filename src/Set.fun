functor Set(M : MAP_BULK) : SET =
struct
  type item = M.key
  type t = unit M.t

  val empty = M.empty
  val member = M.member
  val size = M.size
  val isEmpty = M.isEmpty
  val union = M.union
  val intersect = M.intersect
  val diff = M.diff

  fun singleton k = M.singleton (k, ())
  fun insert s k = M.insert s (k, ())
  fun remove s k =
    let val (removed, s) = M.remove s k
    in
      (Option.isSome removed, s)
    end

end
