open Graphics
open Iterator

(* le type des états de la forme (x, y), (dx, dy)  *)
(* i.e. position (x, y) et vitesse (dx, dy)        *)
type etat = (float * float) * (float * float)

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

  (* a Flux.t-> ('a -> bool) -> ('a -> 'a Flux.t) -> 'a Flux.t *)
let rec unless flux cond f_flux =
  Tick(lazy (match Flux.uncons flux with
  | None -> None
  | Some (t, q) -> if cond t then Flux.uncons (f_flux t)
                    else Some (t, unless q cond f_flux))
  )

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

  module Bouncing (F : Frame) = struct

    module Balle = FreeFall(F)
     
    let contact ((x, y), (dx, dy)) = contact_x F.box_x x dx || contact_y F.box_y y dy
    let rebond ((x, y), (dx, dy)) = (rebond_x F.box_x x dx, rebond_y F.box_y y dy)
    let rec run etat0 = 
      unless (Balle.run etat0) contact (fun (p, v) -> run (p, rebond (p, v)))
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
module D = Drawing (F1)
module FB = Bouncing (F1)
let main () = D.draw(FB.run etat0)