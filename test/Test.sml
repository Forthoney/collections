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

fun fromList xs = List.foldl (fn (k, t) => IntTree.insert t (k, k)) IntTree.empty xs

fun keys t = List.map #1 (IntTree.toList t)

fun assertInorder msg tree expected =
  assertTrue msg (keys tree = expected)

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
val dRight = IntTree.remove tDelRight 4
val _ = assertIntOptionEq "delete on right branch should return deleted value" dRight (SOME 4)

val tDelLeft = fromList [3, 2, 1, 0]
val dLeft = IntTree.remove tDelLeft 0
val _ = assertIntOptionEq "delete on left branch should return deleted value" dLeft (SOME 0)

val dMissing = IntTree.remove tAsc 42
val _ = assertIntOptionEq "deleting missing key should return NONE" dMissing NONE

val tJoinBalanced = IntTree.join (fromList [1, 2]) (3, 3) (fromList [4, 5])
val _ = assertInorder "join should place pivot between left and right trees" tJoinBalanced [1, 2, 3, 4, 5]

val tJoinLeftHeavy = IntTree.join (fromList [1, 2, 3, 4, 5, 6, 7]) (8, 8) (fromList [9])
val _ = assertInorder "join should handle left-heavy rank gap" tJoinLeftHeavy [1, 2, 3, 4, 5, 6, 7, 8, 9]

val tJoinRightHeavy = IntTree.join (fromList [1]) (2, 2) (fromList [3, 4, 5, 6, 7, 8, 9])
val _ = assertInorder "join should handle right-heavy rank gap" tJoinRightHeavy [1, 2, 3, 4, 5, 6, 7, 8, 9]
