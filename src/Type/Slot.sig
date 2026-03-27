signature SLOT =
sig
  type t
  type item
  val empty : t
  val getItem : t -> item option
  val ofItem : item -> t
  val findPlaceholder : t array -> item option
end
