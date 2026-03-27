(* Common, parameterized hashset implementation used by both
   the option-based and bogus-sentinel variants. The SLOT
   structure adapts the representation-specific pieces. *)

functor HashSetCommon(
  structure H : HASHABLE
  structure E : EQ
  structure Slot : SLOT
  sharing type H.t = E.t
  sharing type H.t = Slot.item

  val loadLimit : real
  val growFactor : real
) : IMPERATIVE_SET =
struct
  type item = H.t
  type t = {buffer : Slot.t array ref, size : int ref}
  exception Full

  fun new capacity =
    {buffer = ref (Array.array (capacity, Slot.empty)), size = ref 0}

  fun empty () = new 0

  fun size {buffer, size} = !size
  fun capacity {buffer, size} = Array.length (!buffer)
  fun isEmpty set = size set = 0

  fun loadFactor set = 
    Real.fromInt (size set) / Real.fromInt (capacity set)

  fun grow set =
    Real.round (Real.fromInt (capacity set) * growFactor)

  fun resize (set as {buffer, size}) capacity =
    if capacity <= !size then raise Size
    else
      let val old = !buffer
      in
        buffer := Array.array (capacity, Slot.empty);
        Array.app (Option.app (add set) o Slot.getItem) old
      end

  and probe (set as {buffer, size}) v =
    case !size of
      0 => raise Full
    | size =>
      let
        val pos = Word64.mod (H.hash v, Word64.fromInt size)
        (* conversion is safe because pos < size *)
        val pos = Word64.toInt pos 

        fun search (_, s) =
          case Slot.getItem s of
            SOME v' => E.eq (v, v')
          | NONE => true
        open ArraySlice
      in
        case findi search (slice (!, pos, NONE)) of        
          SOME (i, s) =>
          (case Slot.getItem s of
            NONE => ~i - pos - 1
          | SOME _ => i + pos)
        | NONE =>
          case findi search (slice (!buffer, 0, SOME pos)) of
            SOME (i, s) =>
            (case Slot.getItem s of
              NONE => ~i - 1
            | SOME _ => i)
          | NONE => raise Full
      end

  and add (set as {buffer, size}) v =
    let val i = probe set v
    in
      if i >= 0 then ()
      else if loadFactor set > loadLimit then
        (resize set (grow set); Array.update (!buffer, ~(i + 1), Slot.ofItem v))
      else
        Array.update (!buffer, ~(i + 1), Slot.ofItem v)
    end

  fun member set v = probe set v >= 0

  fun delete (set as {buffer, size}) v =
    let val i = probe set v
    in
      if i < 0 then ()
      else Array.update (!buffer, ~(i + 1), Slot.empty)
    end

  fun app f ({buffer, ...} : t) =
    Array.app (Option.app f o Slot.getItem) (!buffer)

  fun reduce f z ({buffer, ...} : t) =
    Array.foldl (fn (s, acc) =>
      case Slot.getItem s of
        NONE => acc
      | SOME v => f (v, acc)) z (!buffer)

  fun find f ({buffer, ...} : t) =
    let
      fun g s =
        case Slot.getItem s of
          SOME v => f v
        | NONE => false
    in
      case Array.find g (!buffer) of
        SOME s => Slot.getItem s
      | NONE => NONE
    end

  fun filter f ({buffer, ...} : t) =
    Array.modify (fn s => case Slot.getItem s of NONE => s | SOME v => if f v then s else Slot.empty) (!buffer)

  val toList = reduce op:: []

  fun toArray {buffer, size} =
    (case Slot.findPlaceholder (!buffer) of
       SOME placeholder =>
         let
           val a = Array.array (!size, placeholder)
           fun f (s, i) = (case Slot.getItem s of NONE => i | SOME v => (Array.update (a, i, v); i + 1))
         in
           Array.foldl f 0 (!buffer); a
         end
     | NONE => Array.fromList [])

  val toVector = Array.vector o toArray
end

functor HashSet(
  structure H : HASHABLE
  structure E : EQ
  sharing type H.t = E.t

  val loadLimit : real
  val growFactor : real
) : IMPERATIVE_SET =
  HashSetCommon(
    structure H = H
    structure E = E
    structure Slot = struct
      type t = H.t option
      type item = H.t
      val empty = NONE
      fun getItem x = x
      fun ofItem v = SOME v
      fun findPlaceholder arr =
        case Array.find Option.isSome arr of
          SOME (SOME placeholder) => SOME placeholder
        | _ => NONE
    end
    val loadLimit = loadLimit
    val growFactor = growFactor
  )

(* Bogus-sentinel slot adapter *)
functor HashSetBogus(
  structure B : BOGUS
  structure H : HASHABLE
  structure E : EQ
  sharing type B.t = H.t
  sharing type H.t = E.t

  val loadLimit : real
  val growFactor : real
) : IMPERATIVE_SET =
  HashSetCommon(
    structure H = H
    structure E = E
    structure Slot : SLOT = struct
      type t = H.t
      type item = H.t
      val empty = (B.bogus : H.t)
      fun getItem v = if B.isBogus v then NONE else SOME v
      fun ofItem v = v
      fun findPlaceholder _ = SOME B.bogus
    end
    val loadLimit = loadLimit
    val growFactor = growFactor
  )

