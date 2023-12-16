
(* interfaces des flux utiles pour toute la séance *)

module type Iter =
sig
  type 'a t
  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t                        
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end


(* Module Flux implantant l'interface de flux Iter *)
(* a l'aide d'une structure de donnees paresseuse  *)
type 'a flux = Tick of ('a * 'a flux) option Lazy.t;;
module Flux : Iter with type 'a t = 'a flux =
  struct
    type 'a t = 'a flux = Tick of ('a * 'a t) option Lazy.t;;

    let vide = Tick (lazy None);;

    let cons t q = Tick (lazy (Some (t, q)));;

    let uncons (Tick flux) = Lazy.force flux;;
 
    let rec apply f x =
      Tick (lazy (
      match uncons f, uncons x with
      | None         , _             -> None
      | _            , None          -> None
      | Some (tf, qf), Some (tx, qx) -> Some (tf tx, apply qf qx)));;

    let rec unfold f e =
      Tick (lazy (
      match f e with
      | None         -> None
      | Some (t, e') -> Some (t, unfold f e')));;

    let rec filter p flux =
      Tick (lazy (
      match uncons flux with
      | None        -> None
      | Some (t, q) -> if p t then Some (t, filter p q)
                       else uncons (filter p q)));;
    
    let rec append flux1 flux2 =
      Tick (lazy (
      match uncons flux1 with
      | None          -> uncons flux2
      | Some (t1, q1) -> Some (t1, append q1 flux2)));;
    
    let constant c = unfold (fun () -> Some (c, ())) ();;
    (* implantation rapide mais inefficace de map *)
    let map f i = apply (constant f) i;;

    let map2 f i1 i2 = apply (apply (constant f) i1) i2;;
  end


(* le type des états de la forme (x, y), (dx, dy)  *)
(* i.e. position (x, y) et vitesse (dx, dy)        *)
(* On ajoutant l'etat de la raquette*)
type etat = (float * float) * (float * float)


(* Parametres globaux de la simulation      *)
(* dt : pas de temps                        *)
(* box_x : paire d'abscisses (xmin, xmax)   *)
(* box_y : paire d'ordonnees (ymin, ymax)   *)
module type Frame =
  sig
    val dt : float
    val box_x : float * float
    val box_y : float * float
  end

  
(* Fonction qui intègre/somme les valeurs successives du flux *)
(* avec un pas de temps dt et une valeur initiale nulle, i.e. *)
(* acc_0 = 0; acc_{i+1} = acc_{i} + dt * flux_{i}             *)
(* paramètres:                                                *)
(* dt : float                                                 *)
(* flux : (float * float) Flux.t                              *)
let integre dt flux =
  (* valeur initiale de l'intégrateur                         *)
  let init = ( 0., 0.) in
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
  (* définition récursive du flux acc                         *)
  let rec acc =
    Tick (lazy (Some (init, Flux.map2 iter acc flux)))
  in acc;;


(* Module du modèle dynamique d'une balle en 2D.               *)
(* A partir d'un état initial, run produit le flux des états   *)
(* successifs de la balle, qui pourra être affiché             *)
module FreeFall (F : Frame) =
  struct
    (* Exo1 *)
    let run : etat -> etat Flux.t = 
      fun ((p0x, p0y), (v0x, v0y)) ->
      let acceleation = Flux.constant( 0., - 9.81) in 
      let vitesse = Flux.map (fun (a,b) -> (a+. v0x, b+. v0y)) (integre F.dt acceleation) in
      let position = Flux.map (fun (a,b) -> (a+. p0x, b+. p0y)) (integre F.dt vitesse) in
      (* let raquette = Flux.constant (rx, ry) in
      Flux.map2 (fun (p, v) r -> (p, v, r)) (Flux.map2 (fun p v -> (p, v)) position vitesse) raquette *)
      Flux.map2 (fun p v -> (p, v)) position vitesse
  end



(* Exo3*)
(* a Flux.t-> ('a -> bool) -> ('a -> 'a Flux.t) -> 'a Flux.t *)
let rec unless flux cond f_flux =
  Tick(lazy (match Flux.uncons flux with
  | None -> None
  | Some (t, q) -> if cond t then Flux.uncons (f_flux t)
                    else Some (t, unless q cond f_flux))
  )

(* Exo4*)
(*float -> float -> bool*)
let contact_x (x1, x2) x dx = 
  (x <= x1 && dx <= 0.) || (x >= x2 && dx >= 0.)

let contact_y (x1, x2) x dx = 
  (x <= x1 && dx <= 0.) || (x >= x2 && dx >= 0.)

(*float -> float -> float*)
let rebond_x (x1, x2) x dx = 
  if contact_x (x1, x2) x dx then -. dx else dx
  
let rebond_y (x1, x2) y dy = 
  if contact_y (x1, x2) y dy then -. dy else dy


        
(* Exo5*)
module Bouncing (F : Frame) = struct

  module Balle = FreeFall(F)

  let move_paddle ((px, py), (vx, vy), (rx, ry)) new_rx =
    ((px, py), (vx, vy), (new_rx, ry))
   
  let contact ((x, y), (dx, dy)) = contact_x F.box_x x dx || contact_y F.box_y y dy
  let rebond ((x, y), (dx, dy)) = (rebond_x F.box_x x dx, rebond_y F.box_y y dy)
  let rec run etat0 = 
    unless (Balle.run etat0) contact (fun (p, v) -> run (p, rebond (p, v)))
end   

(*Pour la souris: *)
let get_mouse_position () =
  Graphics.mouse_pos ()
        
module BouncingWithPaddle (F : Frame) =
struct
  module Balle = FreeFall(F)

  (* Taille de la raquette *)
  let paddle_width = 80.0
  let paddle_height = 10.0

  type paddle_state = {
    mutable rx : float;
    mutable moving_left : bool;
    mutable moving_right : bool;
  }

  let create_paddle initial_rx = { rx = initial_rx; moving_left = false; moving_right = false }

  let move_paddle paddle new_rx =
    paddle.rx <- new_rx

  let start_move_left paddle =
    paddle.moving_left <- true

  let stop_move_left paddle =
    paddle.moving_left <- false

  let start_move_right paddle =
    paddle.moving_right <- true

  let stop_move_right paddle =
    paddle.moving_right <- false

  let contact ((px, py), _, paddle) =
    py <= (F.box_y |> snd) -. paddle_height /. 2. && py >= (F.box_y |> snd) -. paddle_height

  let rebond ((px, py), (vx, vy), paddle) =
    if contact ((px, py), (vx, vy), paddle) then
      ((px, py), (vx, -.vy), paddle)
    else
      ((px, py), (vx, vy), paddle)

  let update_paddle_with_mouse paddle =
    let mouse_x, _ = get_mouse_position () in
    let paddle_width_half = paddle_width /. 2.0 in
    let new_rx =
      (* La raquette doit rester dans la boîte de jeu *)
      max F.box_x |> fst (min (F.box_x |> snd -. paddle_width) (mouse_x -. paddle_width_half))
    in
    move_paddle paddle new_rx
      
end



(* Module de représentation graphique d'une balle en 2D         *)
(* la simulation s'obtient en appliquant draw à un flux d'états *)
module Drawing (F : Frame) =
  struct
    let draw (r : etat Flux.t) =
      let ref_r = ref r in
      let ref_handler_alrm = ref Sys.(Signal_handle (fun _ -> ())) in
      let ref_handler_int  = ref Sys.(Signal_handle (fun _ -> ())) in
      let handler_alrm i =
        begin
          match Flux.uncons !ref_r with
          | None                          ->
             begin
               Sys.(set_signal sigalrm !ref_handler_alrm);
               Sys.(set_signal sigint  !ref_handler_int)
             end
          | Some (((x, y), (dx, dy)), r') ->
             begin
               (*Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
               Graphics.clear_graph ();
               Graphics.draw_circle (int_of_float x) (int_of_float y) 5;
               Graphics.synchronize ();
               (*ignore (read_line ());*)
               ref_r := r'
             end
        end in
      let handler_int i =
        begin
          ref_r := Flux.vide
        end in
      begin
        let (inf_x, sup_x) = F.box_x in
        let (inf_y, sup_y) = F.box_y in
        let size_x = int_of_float (sup_x -. inf_x) in
        let size_y = int_of_float (sup_y -. inf_y) in
        Graphics.open_graph (Format.sprintf " %dx%d" size_x size_y);
        Graphics.auto_synchronize false;
        Sys.(ref_handler_alrm := signal sigalrm (Signal_handle handler_alrm));
        Sys.(ref_handler_int  := signal sigint  (Signal_handle handler_int));
        Unix.(setitimer ITIMER_REAL { it_interval = F.dt; it_value = F.dt })
      end    
  end

(* Exo2 *)
module F1: Frame = 
struct
  let dt = 0.1
  let box_x = (0., 640.0)
  let box_y = (0., 480.0)
end

let etat0 = ((320.0, 10.0), (40.0, 200.0))
(* module FF = FreeFall(F1)
module D = Drawing (F1)
let main () = D.draw(FF.run etat0) *)

module D = Drawing (F1)
module FB = Bouncing (F1)

let main () = D.draw(FB.run etat0)

