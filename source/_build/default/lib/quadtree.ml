
(********************************************************************************************)
(*module Quadtree : QUADTREE = struct*)

(* Définition du type pour un point *)
type vec = { x : float ; y : float }

(* Création d'un rectangle *)
(* la région de la feuille *)
type rect = {
  ul : vec ; (* coin supérieur gauche *)
  lr : vec   (* coin inférieur droit *)
}

type quadrant =
| NW
| NE
| SW
| SE

(* Vérifie si un point est dans une région *)
let point_inside_region point rect =
  point.x >= rect.ul.x &&
    point.x <= rect.lr.x &&
    point.y >= rect.ul.y &&
    point.y <= rect.lr.y

(* Calcule le centre d'une region *)
let centre_region rect = { x = (rect.ul.x +. rect.lr.x) /. 2. ;
		    y = (rect.ul.y +. rect.lr.y) /. 2. }

(* Définition du type pour un quadtree *)
type 'a quadtree =
  | Leaf of 'a list * rect
  | Node of 'a quadnode

(* Définition du type pour un quadnode *)
and 'a quadnode = {
  centre : vec ; (* centre du rectangle associé à ce noeud *)
  region : rect ;  (* la Grande région ou se trouve le noeud *)
  nw : 'a quadtree ; 
  ne : 'a quadtree ;
  sw : 'a quadtree ;
  se : 'a quadtree ;
}

(* Retourne le rectangle d'un noeud quadtree *)
let region = function
  | Leaf (_,region) -> region
  | Node n -> n.region;;

(* Vérifie si deux régions se chevauchent *)
let superposition_regions r s =
  ( r.ul.x < s.lr.x && r.lr.x > s.ul.x )
  && ( r.ul.y < s.lr.y && r.lr.y > s.ul.y );;
    (* true quand les rectangles r et s se recouvrent *)

(* Création d'un quadtree vide avec région donnée*)
let empty v = Leaf ([],v);;

(* Insère un objet e dans un quadtree *)
let rec insert nlimit e = function
  | Leaf (values, region) ->
    if List.length values + 1 > nlimit 
      then
	let c = centre_region region in
	let n = Node 
	  { centre = c ;
            region = region ;
            nw = empty { ul = region.ul ; lr = c } ;
            sw = empty { ul = { x = region.ul.x ; y = c.y } ; lr = { x = c.x ; y = region.lr.y } };
            se = empty { ul = c ; lr = region.lr } ;
            ne = empty { ul = { x = c.x ; y = region.ul.y } ; lr = { x = region.lr.x ; y = c.y } } }
	in List.fold_right (insert nlimit) values (insert nlimit e n)
      else Leaf (e :: values, region)
  | Node n ->
      let pos = e in
      let node =
	match (pos.x < n.centre.x, pos.y < n.centre.y) with
	    (true, true) -> { n with nw = insert nlimit e n.nw }
	  | (true, false) -> { n with sw = insert nlimit e n.sw }
	  | (false, false) -> { n with se = insert nlimit e n.se }
	  | (false, true) -> { n with ne = insert nlimit e n.ne }
      in Node node;;


let make_leaf () = Leaf ([], { ul = { x = 0.0; y = 0.0 }; lr = { x = 0.0; y = 0.0 } })


let rec create_quadtree frame_width frame_height ball_radius =
  let initial_region = { ul = { x = 0.0; y = 0.0 }; lr = { x = float_of_int(2 * ball_radius); y = float_of_int(2 * ball_radius) } } in
  let initial_quadtree = make_leaf () in
  let rec subdivide region =
    let centre = { x = (region.ul.x +. region.lr.x) /. 2.0; y = (region.ul.y +. region.lr.y) /. 2.0 } in
    let ul = region.ul in
    let lr = region.lr in
    {
      centre;
      region;
      nw = Leaf ([], { ul = { x = ul.x; y = ul.y }; lr = centre });
      ne = Leaf ([], { ul = { x = centre.x; y = ul.y }; lr = { x = lr.x; y = centre.y } });
      sw = Leaf ([], { ul = { x = ul.x; y = centre.y }; lr = { x = centre.x; y = lr.y } });
      se = Leaf ([], { ul = centre; lr = lr });
    }
  in
  let rec divide_tree tree =
    match tree with
    | Leaf (elements, region) ->
        if region.lr.x -. region.ul.x <= frame_width && region.lr.y -. region.ul.y <= frame_height then
          tree
        else
          let divided_node = subdivide region in
          Node {
            centre = divided_node.centre;
            region = divided_node.region;
            nw = divide_tree divided_node.nw;
            ne = divide_tree divided_node.ne;
            sw = divide_tree divided_node.sw;
            se = divide_tree divided_node.se;
          }
    | Node node ->
        Node {
          node with
          nw = divide_tree node.nw;
          ne = divide_tree node.ne;
          sw = divide_tree node.sw;
          se = divide_tree node.se;
        }
  in
  divide_tree initial_quadtree
       

 
  let rec insert_brick_into_region brick region qt =
    match qt with
    | Leaf (bricks, current_region) when current_region = region ->
        Leaf (brick :: bricks, current_region)
    | Node { centre; region = node_region; nw; ne; sw; se } ->
        (* Determiner le quadrant la brique and la region intersect *)
        let intersect_quadrant =
          if region.lr.x <= centre.x && region.lr.y <= centre.y then NW
          else if region.ul.x >= centre.x && region.lr.y <= centre.y then NE
          else if region.ul.y >= centre.y && region.lr.x <= centre.x then SW
          else SE
        in
        (* Recursively insert into the appropriate subquadrant *)
        let updated_subquadrant =
          match intersect_quadrant with
          | NW -> { nw = insert_brick_into_region brick region nw; ne; sw; se; centre; region = node_region }
          | NE -> { nw; ne = insert_brick_into_region brick region ne; sw; se; centre; region = node_region }
          | SW -> { nw; ne; sw = insert_brick_into_region brick region sw; se; centre; region = node_region }
          | SE -> { nw; ne; sw; se = insert_brick_into_region brick region se; centre; region = node_region }
        in
        Node updated_subquadrant
    | _ -> qt   
    

    (* Savoir ou se trouve la brique dans quadtree *)
    (* Collision *)
    (*
    let rec query_brick_region quadtree brick =
      let rec query_node node =
        if is_collision node.region brick then
          match node with
          | Leaf (values, _) -> values
          | Node n -> query_quadnode n
        else
          []
      and query_quadnode { nw; ne; sw; se; _ } =
        List.flatten [query_node nw; query_node ne; query_node sw; query_node se]
      and is_collision region brick =
        (* Vérifie si la brique est en collision avec la région *)
        let brick_ul = { x = brick.x; y = brick.y } in
        let brick_lr = { x = brick.x +. brick.width; y = brick.y +. brick.height } in
        brick_ul.x >= region.ul.x && brick_ul.y >= region.ul.y &&
        brick_lr.x <= region.lr.x && brick_lr.y <= region.lr.y
      in
      match quadtree with
      | Leaf (values, _) -> values
      | Node node -> query_node node
      *)