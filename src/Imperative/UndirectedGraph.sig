signature IMPERATIVE_UNDIRECTED_GRAPH =
sig
  type t
  type vertex
  type edge = vertex * vertex

  val new : int -> t

  structure Vertex :
  sig
    val insert : t -> vertex -> unit
    val delete : t -> vertex -> unit
    val member : t -> vertex -> bool
    val size : t -> int
    val neighbors : t -> vertex -> vertex list
    val degree : t -> vertex -> int
    val toList : t -> vertex list
  end

  structure Edge :
  sig
    val insert : t -> edge -> unit
    val delete : t -> edge -> unit
    val member : t -> edge -> bool
    val size : t -> int
  end
end
