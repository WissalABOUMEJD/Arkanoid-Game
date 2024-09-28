open Graphics
open Iterator

(* Ce fichier est consacré pour la création, la mise à jour de sa position selon la souris et son dessin *)

(* Signature du module Paddle *)
module type PaddleI = sig
  type t

  (* Crée un objet de type 't' avec les coordonnées et dimensions spécifiées *)
  val create : float -> float -> float -> float -> t
  (* Déplace l'objet 't' à la nouvelle position spécifiée *)
  val move_to : t -> float -> unit
  (* Récupère la position x de l'objet 't' *)
  val get_position : t -> float
  (* Met à jour la position de l'objet 't' en suivant la souris *)
  val update_with_mouse : t -> unit
  (* Dessine l'objet 't' à l'écran *)
  val draw_paddle : t -> unit 
  (* Récupère la hauteur de l'objet 't' *)
  val get_h : t -> float
  (* Récupère la largeur de l'objet 't' *)
  val get_w : t -> float
  (* Récupère la coordonnée x de l'objet 't' *)
  val get_x : t -> float
  (* Récupère la coordonnée y de l'objet 't' *)
  val get_y : t -> float
end

(* Implémentation du module Paddle en utilisant la signature PaddleI et dépendant du module Frame 'F' *)
module Paddle (F : Frame) : PaddleI = struct
  type t = {
    mutable x : float;
    mutable y : float;
    width : float;
    height : float;
  }

  (* Récupère la hauteur de l'objet 't' *)
  let get_h paddle =
    paddle.height

  (* Récupère la largeur de l'objet 't' *)
  let get_w paddle =
    paddle.width

  (* Récupère la coordonnée x de l'objet 't' *)
  let get_x paddle =
    paddle.x

  (* Récupère la coordonnée y de l'objet 't' *)
  let get_y paddle =
    paddle.y

  (* Crée un objet de type 't' avec les coordonnées et dimensions spécifiées *)
  let create x y width height = { x; y; width; height }

  (* Déplace l'objet 't' à la nouvelle position spécifiée *)
  let move_to paddle new_x =
    paddle.x <- new_x

  (* Récupère la position x de l'objet 't' *)
  let get_position paddle =
    paddle.x

  (* Met à jour la position de l'objet 't' en suivant la souris *)
  let update_with_mouse paddle =
    let mouse_x, _ = Graphics.mouse_pos () in
    let paddle_width_half = paddle.width /. 2.0 in
    let new_x =
      max (fst F.box_x) (min (snd F.box_x -. paddle.width) (float_of_int mouse_x -. paddle_width_half))
    in
    move_to paddle new_x

  (* Dessine l'objet 't' à l'écran *)
  let draw_paddle paddle =
    set_color red;
    fill_rect (int_of_float paddle.x) (int_of_float paddle.y) (int_of_float paddle.width) (int_of_float paddle.height)
end
