signature MAP_BULK =
sig
  include MAP

  val union : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
end
