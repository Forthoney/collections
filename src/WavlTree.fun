functor WavlTree(O : ORD) : MAP =
struct
  type key = O.t
  datatype parity = Odd | Even
  datatype 'a t = Nil | Node of parity * ('a t * (key * 'a) * 'a t)

  fun getParity Nil = Even
    | getParity (Node (parity, _)) = parity

  fun flip Even = Odd
    | flip Odd = Even

  val demote = flip
  val promote = flip

  datatype dir = L | R

  fun opposite L = R
    | opposite R = L

  fun sides L (l, v, r) = (l, v, r)
    | sides R (l, v, r) = (r, v, l)

  val empty = Nil

  fun balanceIns d parityParent (changed as (parityChanged, bodyChanged)) kv far =
    let
      val (outerChanged, valueChanged, innerChanged) = sides d bodyChanged
    in
      if parityParent <> parityChanged then
        (* changed child was previously a 2-child, now is a 1-child *)
        {promoted = false, node = (parityParent, sides d (Node changed, kv, far))}
      else if parityParent <> getParity far then
        (* changed child is currently a 0-child; unchanged side is a 1-child *)
        {promoted = true, node = (promote parityParent, sides d (Node changed, kv, far))}
      else if parityChanged <> getParity outerChanged then
        (* single rotation *)
        let
          val inner = Node (demote parityParent, sides d (innerChanged, kv, far))
        in
          {promoted = false, node = (parityParent, sides d (outerChanged, valueChanged, inner))}
        end
      else
        (* double rotation *)
        case innerChanged of
          Nil => raise Fail "unreachable"
        | Node (parityInner, bodyInner) =>
          let
            val (outerInner, valueInner, innerInner) = sides d bodyInner
            val near = Node (demote parityChanged, sides d (outerChanged, valueChanged, outerInner))
            val far = Node (demote parityChanged, sides d (innerInner, kv, far))
          in
            {promoted = false, node = (parityParent, sides d (near, valueInner, far))}
          end
    end

  fun ins Nil kv = {promoted = true, node = (Odd, (Nil, kv, Nil))}
    | ins (Node (parity, (l, kv' as (k', _), r))) (kv as (k, _)) =
      case O.compare (k, k') of
        EQUAL => {promoted = false, node = (parity, (l, kv, r))}
      | LESS =>
        (case ins l kv of
          {promoted = true, node} => balanceIns L parity node kv' r
        | {promoted = false, node} =>
          {promoted = false, node = (parity, (Node node, kv', r))})
      | GREATER =>
        (case ins r kv of
          {promoted = true, node} => balanceIns R parity node kv' l
        | {promoted = false, node} =>
          {promoted = false, node = (parity, (l, kv', Node node))})

  fun insert t = Node o #node o ins t

  fun balanceRem d (parent as (parityParent, body)) =
    let
      val (near, kv, far) = sides d body
      val farDir = opposite d
    in
      if getParity near = parityParent then
        (* near side was previously a 1-child, now is a 2-child *)
        case far of
          Nil => {demoted = true, node = Node (demote parityParent, body)}
        | _ => {demoted = false, node = Node parent}
      else if getParity far = parityParent then
        (* near side was previously a 2-child, now is a 3-child; far side is a 2-child *)
        {demoted = true, node = Node (demote parityParent, body)}
      else
        (* near side was previously a 2-child, now is a 3-child; far side is a 1-child *)
        case far of
          Nil => raise Fail "unreachable"
        | Node (parityFar, bodyFar) =>
          let
            val (outerFar, valueFar, innerFar) = sides farDir bodyFar
          in
            if getParity outerFar <> parityFar then
              let
                val near = Node (demote parityParent, sides d (near, kv, innerFar))
              in
                {demoted = false, node = Node (promote parityFar, sides d (near, valueFar, outerFar))}
              end
            else if getParity innerFar <> parityFar then
              case innerFar of
                Nil => raise Fail "unreachable"
              | Node (parityInner, bodyInner) =>
                let
                  val (outerInner, valueInner, innerInner) = sides d bodyInner
                  val near = Node (parityParent, sides d (near, kv, outerInner))
                  val far = Node (demote parityFar, sides d (innerInner, valueFar, outerFar))
                in
                  {demoted = false, node = Node (parityParent, sides d (near, valueInner, far))}
                end
            else
              let val far = Node (demote parityFar, bodyFar)
              in
                {demoted = true, node = Node (demote parityParent, sides d (near, kv, far))}
              end
          end
    end

  fun rem Nil _ = {demoted = false, node = Nil, deleted = NONE}
    | rem (Node (parity, (l, kv' as (k', v), r))) k =
      case O.compare (k, k') of
        EQUAL => {demoted = true, node = r, deleted = SOME v}
      | LESS =>
        let val {demoted, node = l, deleted} = rem l k
        in
          if demoted then
            let val {demoted, node} = balanceRem L (parity, (l, kv', r))
            in
              {demoted = demoted, node = node, deleted = deleted}
            end
          else {demoted = false, node = Node (parity, (l, kv', r)), deleted = deleted}
        end
      | GREATER =>
        let val {demoted, node = r, deleted} = rem r k
        in
          if demoted then
            let val {demoted, node} = balanceRem R (parity, (l, kv', r))
            in
              {demoted = demoted, node = node, deleted = deleted}
            end
          else {demoted = false, node = Node (parity, (l, kv', r)), deleted = deleted}
        end

  fun remove t = #deleted o rem t

  fun parityOfRank r = if r mod 2 = 0 then Even else Odd

  fun rank t =
    let
      fun loop acc Nil = acc
        | loop acc (Node (p, (l, _, _))) =
          loop (acc + (if p = getParity l then 2 else 1)) l
    in
      loop 0 t
    end

  fun joinRight (body as (l, kv, r)) rankL rankR =
    if rankL - rankR <= 1 then
      {promoted = (rankL = rankR), node = (parityOfRank (rankR + 1), body)}
    else
      case l of
        Nil => raise Fail "unreachable"
      | Node (p, (leftL, kvL, rightL)) =>
        let
          val rankRL = rankL - (if p = getParity rightL then 2 else 1)
        in
          case joinRight (rightL, kv, r) rankRL rankR of
            {promoted = true, node} => balanceIns R p node kvL leftL
          | {promoted = false, node} => {promoted = false, node = (p, (leftL, kvL, Node node))}
        end

  fun joinLeft (body as (l, kv, r)) rankL rankR =
    if rankR <= rankL + 1 then
      {promoted = (rankL = rankR), node = (parityOfRank (rankL + 1), body)}
    else
      case r of
        Nil => raise Fail "unreachable"
      | Node (p, (leftR, kvR, rightR)) =>
        let
          val rankLR = rankR - (if p = getParity leftR then 2 else 1)
        in
          case joinLeft (l, kv, leftR) rankL rankLR of
            {promoted = true, node} => balanceIns L p node kvR rightR
          | {promoted = false, node} => {promoted = false, node = (p, (Node node, kvR, rightR))}
        end

  fun join t1 k t2 =
    let
      val r1 = rank t1
      val r2 = rank t2
    in
      if rank t1 >= rank t2 then
        Node (#node (joinRight (t1, k, t2) r1 r2))
      else
        Node (#node (joinLeft (t1, k, t2) r1 r2))
    end

  fun lookup Nil _ = NONE
    | lookup (Node (_, (l, (k', v), r))) k = 
      case O.compare (k, k') of
        EQUAL => SOME v
      | LESS => lookup l k
      | GREATER => lookup r k
  
  fun size Nil = 0
    | size (Node (_, (l, _, r))) = 1 + size l + size r
  
  fun isEmpty Nil = true
    | isEmpty _ = false

  fun toList Nil = []
    | toList (Node (_, (l, kv, r))) = toList l @ (kv :: toList r)

  fun fromList xs = List.foldl (fn (kv, t) => insert t kv) Nil xs
end
