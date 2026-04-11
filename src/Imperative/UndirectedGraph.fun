functor UndirectedGraph(
  structure V : PARTIAL_EQ
  structure S : IMPERATIVE_SET
  sharing type V.t = S.item
) : IMPERATIVE_UNDIRECTED_GRAPH =
struct
  type vertex = V.t
  type edge = vertex * vertex
  type t = {
    vertices : S.t,
    adjacency : (vertex * S.t) list ref,
    edges : int ref
  }

  fun new capacity =
    {vertices = S.new capacity, adjacency = ref [], edges = ref 0}

  fun findNeighbors ({adjacency, ...} : t) v =
    let
      fun loop [] = NONE
        | loop ((v', ns) :: rest) = if V.eq (v, v') then SOME ns else loop rest
    in
      loop (!adjacency)
    end

  fun addVertex ({vertices, adjacency, ...} : t) v =
    if S.member vertices v then ()
    else
      (
        S.insert vertices v;
        adjacency := (v, S.new 0) :: !adjacency
      )

  fun hasEdge (g : t) (u, v) =
    case findNeighbors g u of
      SOME ns => S.member ns v
    | NONE => false

  fun addEdge (g as {edges, ...} : t) (u, v) =
    if hasEdge g (u, v) then ()
    else
      let
        val _ = addVertex g u
        val _ = addVertex g v
      in
        case (findNeighbors g u, findNeighbors g v) of
          (SOME uNeighbors, SOME vNeighbors) =>
            (
              S.insert uNeighbors v;
              if V.eq (u, v) then () else S.insert vNeighbors u;
              edges := !edges + 1
            )
        | _ => raise Fail "UndirectedGraph invariant violated"
      end

  fun removeEdge (g as {edges, ...} : t) (u, v) =
    if not (hasEdge g (u, v)) then ()
    else
      (
        case (findNeighbors g u, findNeighbors g v) of
          (SOME uNeighbors, SOME vNeighbors) =>
            (
              S.delete uNeighbors v;
              if V.eq (u, v) then () else S.delete vNeighbors u;
              edges := !edges - 1
            )
        | _ => ()
      )

  fun removeVertex (g as {vertices, adjacency, ...} : t) v =
    if not (S.member vertices v) then ()
    else
      let
        val incident =
          case findNeighbors g v of
            SOME ns => S.toList ns
          | NONE => []
      in
        List.app (fn u => removeEdge g (v, u)) incident;
        S.delete vertices v;
        adjacency := List.filter (fn (v', _) => not (V.eq (v, v'))) (!adjacency)
      end

  fun neighbors (g : t) v =
    case findNeighbors g v of
      SOME ns => S.toList ns
    | NONE => []

  fun degree (g : t) v =
    case findNeighbors g v of
      SOME ns => S.size ns
    | NONE => 0

  structure Edge =
  struct
    val member = hasEdge
    val delete = removeEdge
    val insert = addEdge

    fun size ({edges, ...} : t) =
      !edges
  end

  structure Vertex =
  struct
    val insert = addVertex
    val delete = removeVertex

    fun member ({vertices, ...} : t) v =
      S.member vertices v

    fun size ({vertices, ...} : t) =
      S.size vertices

    val neighbors = neighbors
    val degree = degree

    fun toList ({vertices, ...} : t) =
      S.toList vertices
  end
end
