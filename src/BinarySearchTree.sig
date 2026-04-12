signature BINARY_SEARCH_TREE =
sig
  include MAP

  val expose : 'a t -> ('a t * (key * 'a) * 'a t) option
end
