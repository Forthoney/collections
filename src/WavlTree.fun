functor WavlTree(O : ORD) =
struct
  datatype parity = Odd | Even
  datatype t = Nil | Node of parity * (t * O.t * t)
  
  fun parity Nil = Even
    | parity (Node (parity, _)) = parity

  fun flip Even = Odd
    | flip Odd = Even

  val demote = flip
  val promote = flip

  type node = parity * (t * O.t * t)
  type insert_result = {promoted : bool, node : node}

  fun balanceL parityParent (l as (parityL, (leftL, valueL, rightL)) : node) v r : insert_result =
    if parityParent <> parityL then
      (* left was previously a 2-child, now is a 1-child *)
      {promoted = false, node = (parityParent, (Node l, v, r))}
    else if parityParent <> parity r then
      (* left is currently a 0-child *)
      (* (unchanged) right is a 1-child. Promoting parent makes parent a 1,2 node *)
      {promoted = true, node = (promote parityParent, (Node l, v, r))}
    else if parityL <> parity leftL then
      (* left is currently a 0-child *)
      (* (unchanged) right is a 2-child. Cannot promote parent *)
      (* leftL is a 1-child. rankParent = rankLeft = rankLeftL + 1 = rankRightL + 2 = rankRight + 2 *)
      (* the resulting node is 1,1 *)
      let
        val inner = Node (demote parityParent, (rightL, v, r))
      in
        {promoted = false, node = (parityParent, (leftL, valueL, inner))}
      end
    else
      (* left is currently a 0-child *)
      (* (unchanged) right is a 2-child. Cannot promote parent *)
      (* leftL is a 2-child. This means that rightL cannot be a Nil since rank Nil <= rank t for any t *)
      case rightL of
        Nil => raise Fail "unreachable"
      | Node (parityRightL, (leftRL, valueRL, rightRL)) =>
        let
          val l = Node (demote parityL, (leftL, valueL, leftRL))
          val r = Node (demote parityL, (rightRL, v, r))
        in
          {promoted = false, node = (parityParent, (l, valueRL, r))}
        end

  fun ins Nil v = {promoted = true, node = (Odd, (Nil, v, Nil))}
    | ins (Node (parity, (l, v', r))) v =
      case O.compare (v, v') of
        EQUAL => {promoted = false, node = (parity, (l, v, r))}
      | LESS =>
        (case ins l v of
          {promoted = true, node} => balanceL parity node v' r
        | {promoted = false, node} =>
          {promoted = false, node = (parity, (Node node, v', r))})
      | GREATER =>
        raise Fail "unimpl"
        (* let val {promoted, node} = ins r v *)
        (* in *)
          (* if promoted then *)
            (* balanceR parity l v' node *)
          (* else *)
            (* {promoted = false, node = (parity, (l, v', node))} *)
        (* end *)

  fun insert t = Node o #node o ins t

  fun balanceL' (parent as (parityParent, body as (l, v, r))) =
    if parity l = parityParent then
      (* left was previously a 1-child, now is a 2-child *)
      (* (unchanged) right was/is a 1/2-child. So, parent is currently 2,2 or 2,1 *)
      (* If parent is a leaf, 2,2 is invalid, so demote the parent. Else, all good *)
      case r of
        Nil => {demoted = true, node = Node (demote parityParent, body)}
      | _ => {demoted = false, node = Node parent}
    else if parity r = parityParent then
      (* left was previously a 2-child, now is a 3-child *)
      (* (unchanged) right was/is a 2-child. Demoting parent makes parent a 2,1 node *)
      {demoted = true, node = Node (demote parityParent, body)}
    else
      (* left was previously a 2-child, now is a 3-child *)
      (* (unchanged) right was/is a 1-child *)
      case r of
        Nil => raise Fail "unreachable" (* since left has rank smaller than right, right cannot be a Nil *)
      | Node (parityR, bodyR as (leftR, valR, rightR)) =>
        if parity rightR <> parityR then (* r is _,1 node. rotate *)
          let val l = Node (demote parityParent, (l, v, leftR))
          in
            {demoted = false, node = Node (promote parityR, (l, valR, rightR))}
          end
        else if parity leftR <> parityR then (* r is 1,2 node. double rotate *)
          case leftR of
            Nil => raise Fail "unreachable" (* since rightR has rank smaller than leftR, leftR cannot be a Nil *)
          | Node (parityLR, (leftLR, valLR, rightLR)) =>
            let
              val l = Node (parityParent, (l, v, leftLR)) (* demote parent twice *)
              val r = Node (demote parityR, (rightLR, valR, rightR))
            in
              {demoted = false, node = Node (parityParent, (l, valLR, r))}
            end
        else
          (* right is 2,2 *)
          let val r = Node (demote parityR, bodyR)
          in
            {demoted = true, node = Node (demote parityParent, (l, v, r))}
          end

  fun del Nil v = {demoted = false, node = Nil}
    | del (node as Node (parity, (l, v', r))) v =
      case O.compare (v, v') of
        EQUAL => {demoted = true, node = r}
      | LESS =>
        let val {demoted, node = l} = del l v
        in
          if demoted then balanceL' (parity, (l, v', r))
          else {demoted = false, node = Node (parity, (l, v', r))}
        end
      | GREATER =>
        raise Fail "unimpl"

  fun delete t = #node o del t
end
