structure IntOrd = Ord(
  type t = int
  val compare = Int.compare
)

structure IntTree = WavlTree(IntOrd)

fun fail msg = raise Fail msg

fun assertTrue msg cond =
  if cond then
    ()
  else
    fail msg

fun inorder IntTree.Nil = []
  | inorder (IntTree.Node (_, (l, (k, _), r))) = inorder l @ (k :: inorder r)

fun fromList xs = List.foldl (fn (k, t) => IntTree.insert t (k, k)) IntTree.empty xs

fun assertInorder msg tree expected =
  assertTrue msg (inorder tree = expected)

fun assertIntOptionEq msg actual expected =
  assertTrue msg (actual = expected)

val tDesc = fromList [3, 2, 1]
val tAsc = fromList [1, 2, 3]
val tLeftRight = fromList [3, 1, 2]
val tRightLeft = fromList [1, 3, 2]

val _ = assertInorder "descending insert should preserve sorted order" tDesc [1, 2, 3]
val _ = assertInorder "ascending insert should preserve sorted order" tAsc [1, 2, 3]
val _ = assertInorder "left-right insert pattern should preserve sorted order" tLeftRight [1, 2, 3]
val _ = assertInorder "right-left insert pattern should preserve sorted order" tRightLeft [1, 2, 3]

val tDelRight = fromList [2, 1, 3, 4]
val dRight = IntTree.delete tDelRight 4
val _ = assertIntOptionEq "delete on right branch should return deleted value" dRight (SOME 4)

val tDelLeft = fromList [3, 2, 1, 0]
val dLeft = IntTree.delete tDelLeft 0
val _ = assertIntOptionEq "delete on left branch should return deleted value" dLeft (SOME 0)

val dMissing = IntTree.delete tAsc 42
val _ = assertIntOptionEq "deleting missing key should return NONE" dMissing NONE
