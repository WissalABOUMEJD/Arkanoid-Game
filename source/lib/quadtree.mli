type vec = { x : float; y : float }

type rect = {
  ul : vec;  (* coin supérieur gauche upperLeft*)
  lr : vec   (* coin inférieur droit  lowerRight*)
}

type 'a quadtree =
  | Leaf of vec list * rect
  | Node of 'a quadnode

and 'a quadnode = {
  centre : vec;
  region : rect;
  nw : 'a quadtree;
  ne : 'a quadtree;
  sw : 'a quadtree;
  se : 'a quadtree;
}

val point_inside_region : vec -> rect -> bool
val centre_region : rect -> vec
val region : 'a quadtree -> rect
val superposition_regions : rect -> rect -> bool
val empty : rect -> 'a quadtree
val insert : int -> vec -> 'a quadtree -> 'a quadtree

