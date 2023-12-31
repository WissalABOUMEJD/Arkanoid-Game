open Graphics

(*
  Ce module permet de créer des briques et de les dessiner.
  Une brique est représentée par un rectangle de largeur et de hauteur données.
  La position de la brique est donnée par les coordonnées de son coin supérieur gauche.
  La brique est dessinée avec une couleur différente selon son niveau.
  Le niveau d'une brique est un entier entre 1 et 4.
  Le niveau d'une brique détermine sa couleur.
*)

module type BrickInterf =
  sig
    type t = { x : float; y : float; width : float; height : float; level : int ; mutable visible : bool}
    val create : float -> float -> float -> float -> int -> t
    val draw : t -> unit
  end 

module Brick : BrickInterf = 
struct
  type t = { x : float; y : float; width : float; height : float; level : int ; mutable visible : bool}
  let create x y width height level = { x; y; width; height; level; visible = true }

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

    (*
(* Fonction pour vérifier la collision entre la balle et la brique *)
let check_collision ball_x ball_y ball_radius brick =
  let brick_x = brick.x in
  let brick_y = brick.y in
  let brick_width = brick.width in
  let brick_height = brick.height in

  let ball_left = ball_x -. ball_radius in
  let ball_right = ball_x +. ball_radius in
  let ball_top = ball_y -. ball_radius in
  let ball_bottom = ball_y +. ball_radius in

  let brick_left = brick_x in
  let brick_right = brick_x +. brick_width in
  let brick_top = brick_y in
  let brick_bottom = brick_y +. brick_height in

  ball_right >= brick_left && ball_left <= brick_right &&
  ball_bottom >= brick_top && ball_top <= brick_bottom

(* Fonction pour gérer la collision avec les briques *)
let handle_collision_with_bricks ball_x ball_y ball_radius bricks =
  let rec handle_collision_ball_with_brick bricks =
    match bricks with
    | [] -> ()  (* Aucune collision détectée *)
    | brick :: rest ->
        if brick.visible && check_collision ball_x ball_y ball_radius brick then begin
          (* Collision détectée, traiter la disparition de la brique *)
          brick.visible <- false;
          handle_collision_ball_with_brick rest
        end else
          handle_collision_ball_with_brick rest
  in

  handle_collision_ball_with_brick bricks
*)
end

    
let brick_width = 45
let brick_height = 10

let main () =
  open_graph " 800x600";
  auto_synchronize false; (* Disable automatic synchronization for faster drawing *)

  (* Create and draw the bricks *)
  let brick1 = Brick.create 500.0 300.0 (float brick_width) (float brick_height) 1 in
  let brick2 = Brick.create 500.0 350.0 (float brick_width) (float brick_height) 2 in
  let brick3 = Brick.create 500.0 400.0 (float brick_width) (float brick_height) 3 in
  let brick4 = Brick.create 500.0 450.0 (float brick_width) (float brick_height) 4 in

  let brick5 = Brick.create 200.0 300.0 (float brick_width) (float brick_height) 1 in
  let brick6 = Brick.create 200.0 350.0 (float brick_width) (float brick_height) 2 in
  let brick7 = Brick.create 200.0 400.0 (float brick_width) (float brick_height) 3 in
  let brick8 = Brick.create 200.0 450.0 (float brick_width) (float brick_height) 4 in

  let brick9 = Brick.create 300.0 300.0 (float brick_width) (float brick_height) 1 in
  let brick10 = Brick.create 300.0 350.0 (float brick_width) (float brick_height) 2 in
  let brick11 = Brick.create 300.0 400.0 (float brick_width) (float brick_height) 3 in
  let brick12 = Brick.create 300.0 450.0 (float brick_width) (float brick_height) 4 in

  let brick13 = Brick.create 400.0 300.0 (float brick_width) (float brick_height) 1 in
  let brick14 = Brick.create 400.0 350.0 (float brick_width) (float brick_height) 2 in
  let brick15 = Brick.create 400.0 400.0 (float brick_width) (float brick_height) 3 in
  let brick16 = Brick.create 400.0 450.0 (float brick_width) (float brick_height) 4 in

  let brick17 = Brick.create 100.0 300.0 (float brick_width) (float brick_height) 1 in
  let brick18 = Brick.create 100.0 350.0 (float brick_width) (float brick_height) 2 in
  let brick19 = Brick.create 100.0 400.0 (float brick_width) (float brick_height) 3 in
  let brick20 = Brick.create 100.0 450.0 (float brick_width) (float brick_height) 4 in

  let brick22 = Brick.create 600.0 300.0 (float brick_width) (float brick_height) 1 in
  let brick23 = Brick.create 600.0 350.0 (float brick_width) (float brick_height) 2 in
  let brick24 = Brick.create 600.0 400.0 (float brick_width) (float brick_height) 3 in
  let brick25 = Brick.create 600.0 450.0 (float brick_width) (float brick_height) 4 in
 
  let bricks = [brick1; brick2; brick3; brick4; brick5; brick6; brick7; brick8; brick9; brick10; brick11; brick12; brick13; 
  brick14; brick15; brick16; brick17; brick18; brick19; brick20; brick22; brick23; brick24; brick25] in
  
  List.iter Brick.draw bricks;

  synchronize (); (* Synchronize the drawing *)
  ignore (wait_next_event [Key_pressed]); (* Wait for a key press before closing *)

  close_graph ()

let () = main ()
