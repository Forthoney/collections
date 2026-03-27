signature HASHABLE =
sig
  type t
  val hash : t -> Word64.word
end
