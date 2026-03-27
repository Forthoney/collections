functor DynArrayCommon(
  structure Slot : SLOT
  val growFactor : real
) : DYN_ARRAY =
struct
  type item = Slot.item
  type t = {buffer : Slot.t array ref, size : int ref}

  fun new capacity =
    {buffer = ref (Array.array (capacity, Slot.empty)), size = ref 0}

  fun empty () = new 0

  fun size ({size, ...} : t) = !size
  fun capacity ({buffer, ...} : t) = Array.length (!buffer)
  fun isEmpty set = size set = 0

  fun loadFactor {buffer, size} = 
    Real.fromInt (!size) / Real.fromInt (Array.length (!buffer))

  fun grow arr =
    Real.round (Real.fromInt (capacity arr) * growFactor)

  fun push (arr as {buffer, size}) v =
    ( if capacity arr >= !size then resize arr (grow arr) else ()
    ; Array.update (!buffer, !size, v)
    ; size := !size + 1
    )
end
