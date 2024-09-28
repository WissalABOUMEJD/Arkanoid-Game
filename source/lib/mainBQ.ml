open Graphics
open Quadtree


module type BrickInterf =
  sig
    type t = { x : float; y : float; width : float; height : float; level : int ; mutable visible : bool}
    val create : float -> float -> float -> float -> int -> t
    val draw : t -> unit
  end 

module Brick : BrickInterf = 
struct
  type t = { x : float; y : float; width : float; height : float; level : int ; mutable visible : bool}
  let create x y width height level = { x; y; width; height; level ; visible = true}

  
  let draw brick =
    let color =
      if brick.visible then (
        match brick.level with
        | 1 -> Graphics.green
        | 2 -> Graphics.red
        | 3 -> Graphics.blue
        | 4 -> Graphics.yellow
        | _ -> Graphics.black 
      ) else (
        Graphics.white (* Briques invisibles *)
      )
    in
    Graphics.set_color color;
    Graphics.fill_rect (int_of_float brick.x) (int_of_float brick.y)
      (int_of_float brick.width) (int_of_float brick.height)
  
end

(************************************************************************************************************************************************************)
include Brick
let brick_width = 60
let brick_height = 25

type frame = {
  width : float;
  height : float;
}

(* Les fonctions après sont utilisés pour la quadtree de notre jeu et l'insertion des briques dedans *)

let make_leaf (region : rect) : 'a quadtree =
  Leaf ([], region)

let centre_region (region : rect) : vec =
  { x = (region.ul.x +. region.lr.x) /. 2.0; y = (region.ul.y +. region.lr.y) /. 2.0 }

let rec insert_quadtree (quadtree : 'a quadtree) (item : 'a) : 'a quadtree =

  match quadtree with
  | Leaf (values, region) ->
      (*let updated_values = item :: values in*)
      (* Création d'un noeud initial si la feuille est pleine *)
      if List.length values + 1 > 1 then
        let c = centre_region region in
        let n = Node {
              centre = c;
              region = region;
              nw = make_leaf { ul = region.ul; lr = c };
              ne = make_leaf { ul = { x = c.x ; y = region.ul.y } ; lr = { x = region.lr.x ; y = c.y } }; 
              sw = make_leaf { ul = { x = region.ul.x; y = c.y }; lr = c };
              se = make_leaf { ul = c; lr = region.lr };
            }
        
      in List.fold_right (fun v acc -> insert_quadtree acc v) values (insert_quadtree n item)
      else
        
        Leaf (item :: values, region)
  | Node n ->
      (* Insértion dans le noeud *)
      let pos = { x = item.x; y = item.y } in
        let updated_node =
          match (pos.x < n.centre.x, pos.y < n.centre.y) with
          | (true, true) -> { n with nw = insert_quadtree n.nw item }
          | (true, false) -> { n with sw = insert_quadtree n.sw item }
          | (false, false) -> { n with se = insert_quadtree n.se item }
          | (false, true) -> { n with ne = insert_quadtree n.ne item }
        in
        Node updated_node

(********************************************************************)
(* Création de la quadtree *)
  let create_quadtree frame_width frame_height : 'a quadtree =
  let initial_region = { ul = { x = 0.0; y = frame_height/. 2.0  }; lr = { x = frame_width; y = frame_height } } in
  let centre = centre_region initial_region in
  let initial_node = {
    centre = centre_region initial_region;
    region = initial_region;
    nw = make_leaf { ul = initial_region.ul; lr = centre };
    ne = make_leaf { ul = { x = centre.x; y = initial_region.ul.y }; lr = { x = initial_region.lr.x; y = centre.y } };
    sw = make_leaf { ul = { x = initial_region.ul.x; y = centre.y }; lr = centre };
    se = make_leaf { ul = centre; lr = initial_region.lr };
  } in
  Node initial_node


(***********************************************************************)
(* Affichage de la quadtree en console *)
let rec print_quadtree quadtree =
  match quadtree with
  | Leaf (values, region) ->
    Printf.printf "Leaf with %d values in region (%.2f, %.2f) to (%.2f, %.2f)\n"
      (List.length values) region.ul.x region.ul.y region.lr.x region.lr.y
  | Node n ->
    Printf.printf "Node with centre (%.2f, %.2f) in region (%.2f, %.2f) to (%.2f, %.2f)\n"
      n.centre.x n.centre.y n.region.ul.x n.region.ul.y n.region.lr.x n.region.lr.y;
    print_quadtree n.nw;
    print_quadtree n.ne;
    print_quadtree n.sw;
    print_quadtree n.se


(********************************************************************************************)
(* Récupération des briques du quadtree *)
let rec get_briques quadtree =
  match quadtree with
  | Leaf (briques, _) -> briques
  | Node n ->
    (* Récupération des briques de chaque sous-arbre *)
    let briques_nw = get_briques n.nw in
    let briques_ne = get_briques n.ne in
    let briques_sw = get_briques n.sw in
    let briques_se = get_briques n.se in
    briques_nw @ briques_ne @ briques_sw @ briques_se

  
(*****************************************************************************)
(* Création des briques pour le jeu *)

let create_bricks () = 

let brick1 = Brick.create 220.0 340.0 (float brick_width) (float brick_height) 1 in
let brick2 = Brick.create 220.0 380.0 (float brick_width) (float brick_height) 2 in
let brick3 = Brick.create 220.0 420.0 (float brick_width) (float brick_height) 3 in

let brick5 = Brick.create 140.0 340.0 (float brick_width) (float brick_height) 1 in
let brick6 = Brick.create 140.0 380.0 (float brick_width) (float brick_height) 2 in
let brick8 = Brick.create 140.0 460.0 (float brick_width) (float brick_height) 4 in

let brick13 = Brick.create 405.0 340.0 (float brick_width) (float brick_height) 1 in
let brick14 = Brick.create 405.0 380.0 (float brick_width) (float brick_height) 2 in
let brick15 = Brick.create 405.0 460.0 (float brick_width) (float brick_height) 3 in

let brick17 = Brick.create 605.0 340.0 (float brick_width) (float brick_height) 1 in
let brick18 = Brick.create 605.0 380.0 (float brick_width) (float brick_height) 2 in
let brick19 = Brick.create 605.0 420.0 (float brick_width) (float brick_height) 3 in
let brick20 = Brick.create 605.0 460.0 (float brick_width) (float brick_height) 4 in


let brick22 = Brick.create 310.0 340.0 (float brick_width) (float brick_height) 1 in
let brick23 = Brick.create 310.0 380.0 (float brick_width) (float brick_height) 2 in
let brick24 = Brick.create 310.0 420.0 (float brick_width) (float brick_height) 3 in
let brick25 = Brick.create 310.0 460.0 (float brick_width) (float brick_height) 4 in

let brick26 = Brick.create 510.0 340.0 (float brick_width) (float brick_height) 1 in
let brick27 = Brick.create 510.0 380.0 (float brick_width) (float brick_height) 2 in

let brick30 = Brick.create 710.0 340.0 (float brick_width) (float brick_height) 1 in
let brick31 = Brick.create 710.0 380.0 (float brick_width) (float brick_height) 2 in 
let brick32 = Brick.create 710.0 420.0 (float brick_width) (float brick_height) 3 in
let brick33 = Brick.create 710.0 460.0 (float brick_width) (float brick_height) 4 in


  let bricks = [brick1;brick2;brick3; brick5; brick6; brick13;brick14; brick15; brick17; brick18; brick19; brick22; brick23; brick24; brick26; brick27; brick30; brick31; brick32] in bricks
(*****************************************************************************)

(* Création de la quadtree pour le jeu *)
let createQ x y =
  let quadtree = create_quadtree x y in

  (* Création des briques *)
  let bricks = create_bricks () in

  (* Insertion des briques dans le quadtree *)
  let quadtree_with_bricks = List.fold_left insert_quadtree quadtree bricks in
  quadtree_with_bricks

let rec draw_quadtree qt =
    match qt with
    | Leaf (elements, region) ->
        (* Dessin de la région *)
        (*set_color blue;*)
        (* draw_rect (int_of_float region.ul.x) (int_of_float region.ul.y)
                  (int_of_float (region.lr.x -. region.ul.x))
                  (int_of_float (region.lr.y -. region.ul.y));
      *)
        (* Dessin des briques *)
      List.iter Brick.draw elements
    | Node n ->
        (* Dessin de la région du nœud *)
        (*set_color green;
        draw_rect (int_of_float n.region.ul.x) (int_of_float n.region.ul.y)
                  (int_of_float (n.region.lr.x -. n.region.ul.x))
                  (int_of_float (n.region.lr.y -. n.region.ul.y));
        *)
        (* Dessin des sous-quadtrees *)
        draw_quadtree n.nw;
        draw_quadtree n.ne;
        draw_quadtree n.sw;
        draw_quadtree n.se