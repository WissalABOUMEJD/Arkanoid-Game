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
        match brick.level with
        | 1 -> Graphics.green
        | 2 -> Graphics.red
        | 3 -> Graphics.blue
        | 4 -> Graphics.yellow
        | _ -> Graphics.black (* Couleur par défaut pour les niveaux non spécifiés *)
      in
      Graphics.set_color color;
      Graphics.fill_rect (int_of_float brick.x) (int_of_float brick.y)
        (int_of_float brick.width) (int_of_float brick.height)
end

(* Collision 

let collision ball_rect brick_rect =
  let ball_left = ball_rect.ul.x in
  let ball_right = ball_rect.lr.x in
  let ball_top = ball_rect.ul.y in
  let ball_bottom = ball_rect.lr.y in

  let brick_left = brick_rect.ul.x in
  let brick_right = brick_rect.lr.x in
  let brick_top = brick_rect.ul.y in
  let brick_bottom = brick_rect.lr.y in

  ball_left <= brick_right && ball_right >= brick_left &&
  ball_top <= brick_bottom && ball_bottom >= brick_top


let collision_result brick_rect =
  let ball_rect = {
    ul = { x = ball.x -. ball_radius; y = ball.y -. ball_radius };
    lr = { x = ball.x +. ball_radius; y = ball.y +. ball_radius };
  } in
  collision ball_rect brick_rect


let find_and_remove_collided_brick state collision_result =
  let rec find_and_remove_brick qt =
    match qt with
    | Quadtree.Leaf (bricks, _) ->
        let collided_brick_opt =
          List.find_opt
            (fun brick ->
              let brick_rect =
                {
                  Quadtree.ul = { x = brick.Brick.x; y = brick.Brick.y };
                  lr = { x = brick.Brick.x +. brick.Brick.width; y = brick.Brick.y +. brick.Brick.height };
                }
              in
              collision_result brick_rect
            )
            bricks
        in
        (match collided_brick_opt with
        | Some(collided_brick) ->
            (* Supprimer la brique touchée de la liste *)
            state.bricks <- List.filter (fun brick -> brick != collided_brick) state.bricks
        | None -> ());
    | Quadtree.Node (nw, ne, sw, se) ->
        find_and_remove_brick nw;
        find_and_remove_brick ne;
        find_and_remove_brick sw;
        find_and_remove_brick se
  in

  find_and_remove_brick state.quadtree

  *)

(************************************************************************************************************************************************************)
include Brick
let brick_width = 45
let brick_height = 10

type frame = {
  width : float;
  height : float;
}

let make_leaf (region : rect) : 'a quadtree =
  Leaf ([], region)

let centre_region (region : rect) : vec =
  { x = (region.ul.x +. region.lr.x) /. 2.0; y = (region.ul.y +. region.lr.y) /. 2.0 }

let rec insert_quadtree (quadtree : 'a quadtree) (item : 'a) : 'a quadtree =

  (*let is_upper_frame_brick (brick : 'a) =
    brick.y >= 300.0 /. 2.0 && brick.y <= 600.0 
  in*)

  match quadtree with
  | Leaf (values, region) ->
      let updated_values = item :: values in
      (* Créer un nœud initial si la feuille est pleine *)
      if List.length values + 1 > 1 then
        let c = centre_region region in
        let n = Node {
              centre = c;
              region = region;
              nw = make_leaf { ul = region.ul; lr = c };
              ne = make_leaf { ul = { x = region.ul.x; y = c.y }; lr = { x = c.x; y = region.lr.y } };
              sw = make_leaf { ul = { x = region.ul.x; y = c.y }; lr = c };
              se = make_leaf { ul = c; lr = region.lr };
            }
        in
        List.fold_left (fun acc v -> insert_quadtree acc v) n updated_values
      else
        (* Ajouter à la feuille existante *)
        Leaf (updated_values, region)
  | Node n ->
      (* Insérer dans le nœud *)
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

(*
(* Retourne la région de l'objet (brique par exemple)*)
let rec find_region (quadtree : 'a quadtree) (item : 'a) : rect option =
  match quadtree with
  | Leaf (_, region) -> Some region
  | Node n ->
      let pos = { x = item.x; y = item.y } in
      match (pos.x < n.centre.x, pos.y < n.centre.y) with
      | (true, true) -> find_region n.nw item
      | (true, false) -> find_region n.sw item
      | (false, false) -> find_region n.se item
      | (false, true) -> find_region n.ne item

let rec remove_brick_from_quadtree (quadtree : 'a quadtree) (item : 'a) : 'a quadtree =
  match quadtree with
  | Leaf (values, region) ->
      let updated_values = List.filter (fun v -> v != item) values in
      if List.length updated_values = 0 then
        make_leaf region
      else
        Leaf (updated_values, region)
  | Node n -> 
      let pos = { x = item.x; y = item.y } in
      let updated_node =
        match (pos.x < n.centre.x, pos.y < n.centre.y) with
        | (true, true) -> { n with nw = remove_brick_from_quadtree n.nw item }
        | (true, false) -> { n with sw = remove_brick_from_quadtree n.sw item }
        | (false, false) -> { n with se = remove_brick_from_quadtree n.se item }
        | (false, true) -> { n with ne = remove_brick_from_quadtree n.ne item }
      in
      Node updated_node

    let update_quadtree_after_collision quadtree brick =
      match find_region quadtree brick with
        | Some region ->
            let updated_quadtree = remove_brick_from_quadtree quadtree brick in
            draw_quadtree_with_bricks updated_quadtree;
            updated_quadtree
        | None -> quadtree

    *)

(*
  let rec is_collision region x y width height =
  let open Float in
  let brick_region = { ul = { x = x; y = y }; lr = { x = x +. width; y = y +. height } } in
  region.ul.x <= brick_region.lr.x && region.lr.x >= brick_region.ul.x && region.ul.y <= brick_region.lr.y
  && region.lr.y >= brick_region.ul.y

  let update_quadtree_after_collision quadtree brick =
  match find_region quadtree brick with
  | Some region ->
      let updated_quadtree = remove_brick_from_quadtree quadtree brick in
      draw_quadtree_with_bricks updated_quadtree;
      updated_quadtree
  | None -> quadtree
*)



let main () =
  (* Initialiser la fenêtre graphique *)
  open_graph " 800x600";
  auto_synchronize false; (* Disable automatic synchronization for faster drawing *)
  (*set_color Graphics.green;*)
  let frame_width = 800.0 in
  let frame_height = 600.0 in
  let frame_bottom = frame_height /. 2.0 in
  let frame_top = frame_height in


  let region_init = { ul = { x = 0.0; y = 300.0 }; lr = { x = 800.0; y = 600.0 } } in

  
  (* Créer le quadtree *)
  let quadtree = create_quadtree 800.0 600.0 in
  (*let quadtree = make_leaf region_init in*)

  (* Créer les briques *)
            
 let brick1 = Brick.create 205.0 310.0 (float brick_width) (float brick_height) 1 in
 (*NW*)
 let brick2 = Brick.create 205.0 350.0 (float brick_width) (float brick_height) 2 in
 let brick3 = Brick.create 205.0 400.0 (float brick_width) (float brick_height) 3 in
 let brick4 = Brick.create 205.0 470.0 (float brick_width) (float brick_height) 4 in  
 

 let brick5 = Brick.create 130.0 310.0 (float brick_width) (float brick_height) 1 in
 let brick6 = Brick.create 130.0 350.0 (float brick_width) (float brick_height) 2 in
 let brick7 = Brick.create 130.0 400.0 (float brick_width) (float brick_height) 3 in
 let brick8 = Brick.create 130.0 470.0 (float brick_width) (float brick_height) 4 in
 
 
 let brick9 = Brick.create 30.0 310.0 (float brick_width) (float brick_height) 1 in
 let brick10 = Brick.create 30.0 350.0 (float brick_width) (float brick_height) 2 in
 let brick11 = Brick.create 30.0 400.0 (float brick_width) (float brick_height) 3 in
 let brick12 = Brick.create 30.0 470.0 (float brick_width) (float brick_height) 4 in

 let brick13 = Brick.create 500.0 310.0 (float brick_width) (float brick_height) 1 in
  (*NW*)
 let brick14 = Brick.create 500.0 350.0 (float brick_width) (float brick_height) 2 in
 let brick15 = Brick.create 500.0 400.0 (float brick_width) (float brick_height) 3 in
 let brick16 = Brick.create 500.0 470.0 (float brick_width) (float brick_height) 4 in

 let brick17 = Brick.create 600.0 310.0 (float brick_width) (float brick_height) 1 in
  (*NW*)
 let brick18 = Brick.create 600.0 350.0 (float brick_width) (float brick_height) 2 in
 let brick19 = Brick.create 600.0 400.0 (float brick_width) (float brick_height) 3 in
 let brick20 = Brick.create 600.0 430.0 (float brick_width) (float brick_height) 4 in
 let brick21 = Brick.create 600.0 470.0 (float brick_width) (float brick_height) 5 in

 (*NW*)
let brick22 = Brick.create 300.0 310.0 (float brick_width) (float brick_height) 1 in
 let brick23 = Brick.create 300.0 350.0 (float brick_width) (float brick_height) 2 in
 let brick24 = Brick.create 300.0 400.0 (float brick_width) (float brick_height) 3 in
 let brick25 = Brick.create 300.0 470.0 (float brick_width) (float brick_height) 4 in


let bricks = [brick1;brick3;brick4; brick5; brick6; brick7; brick8; brick9; brick10; brick11; brick12; brick13; brick15; brick16; brick17; brick19; brick20; 
brick21] in 


(* Insertion des briques dans le quadtree *)
let quadtree_with_bricks = List.fold_left insert_quadtree quadtree bricks in
(*

(* Recherche de la région d'une brique dans le quadtree *)
let brick_to_find = List.hd bricks in
let region_of_brick = find_region quadtree_with_bricks brick_to_find in 
match region_of_brick with
| Some region ->
    (* Affichage des coordonnées de la région dans le terminal *)
    Printf.printf "Région de la brique : ul(%.2f, %.2f), lr(%.2f, %.2f)\n"
    region.ul.x region.ul.y region.lr.x region.lr.y;
| None -> ();;
*)

(*
let draw_ball ball =
  set_color blue;
  fill_circle (int_of_float ball.x) (int_of_float ball.y) (int_of_float ball.radius)

let draw_element element =
  match element with
  | `Brick brick -> draw_brick brick
  | `Ball ball -> draw_ball ball
*)

let rec draw_quadtree qt =
  match qt with
  | Leaf (elements, region) ->
      (* Dessiner la région *)
      set_color blue;
      draw_rect (int_of_float region.ul.x) (int_of_float region.ul.y)
                (int_of_float (region.lr.x -. region.ul.x))
                (int_of_float (region.lr.y -. region.ul.y));

      (* Dessiner les éléments de la feuille *)
      (*
      set_color red;
      (*List.iter draw_element elements*)
      List.iter (fun e -> fill_rect (int_of_float e.x) (int_of_float e.y) 5 5) elements
      *)
      List.iter Brick.draw elements
  | Node n ->
      (* Dessiner la région du nœud *)
      set_color green;
      draw_rect (int_of_float n.region.ul.x) (int_of_float n.region.ul.y)
                (int_of_float (n.region.lr.x -. n.region.ul.x))
                (int_of_float (n.region.lr.y -. n.region.ul.y));

      (* Dessiner les sous-quadtrees *)
      draw_quadtree n.nw;
      draw_quadtree n.ne;
      draw_quadtree n.sw;
      draw_quadtree n.se 
in
draw_quadtree quadtree_with_bricks;

  (* Attendre un événement utilisateur avant de fermer la fenêtre *)
  synchronize (); (* Synchronize the drawing *)
  ignore (wait_next_event [Key_pressed]); (* Wait for a key press before closing *)
  
    (* Fermer la fenêtre *)
  close_graph ()


(* Appeler la fonction main *)
let () = main ()
