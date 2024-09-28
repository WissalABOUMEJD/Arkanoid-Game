open Paddle
open Iterator
open Graphics

(* le type des états de la forme (x, y), (dx, dy)  *)
(* i.e. position (x, y) et vitesse (dx, dy)        *)
type etat = (float * float) * (float * float)

(*Module de configuration de la raquette*)
module Config = 
struct
  
  let x = 300.0
  let y = 45.0
  let  width = 100.0
  let height = 10.0
  
  
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
    let rB = 5.0
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
(*   let (rx,_) = mouse_pos () in 
  (x <= x1 && dx <= 0.) || (x >= x2 && dx >= 0.) || (x >= float_of_int rx && x <= float_of_int rx +. Config.width && dx < 0.)  *)
  (x <= x1 && dx <= 0.) || (x >= x2 && dx >= 0.)

let contact_y (x1, x2) x dx = 
(*   (x <= x1 && dx <= 0.) || (x >= x2 && dx >= 0.) || (x >= Config.y && x <= Config.y +. Config.height && dx < 0. )
 *)   (x <= x1 && dx <= 0.) || (x >= x2 && dx >= 0.) 
 
(*float -> float -> float*)
let rebond_x (x1, x2) x dx = 
  if (contact_x (x1, x2) x dx) then -. dx else dx
  
let rebond_y (x1, x2) y dy = 
  (* if (contact_y (x1, x2) y dy) || (contact_paddle_y (x1, x2) y dy) then -. dy else dy *)
  if (contact_y (x1, x2) y dy) then -. dy else dy
