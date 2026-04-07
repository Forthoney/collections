signature PRIORITY_QUEUE =
sig
  include CONTAINER

  val peek : t -> item option
  val pop : t -> (item * t) option
  val merge : t * t -> t
end
