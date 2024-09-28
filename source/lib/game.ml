
(* let game_hello () = print_endline "Hello, Newtonoiders!"
 *)

 open Graphics
 open Iterator
 open Ball
 open Paddle
 open MainBQB
 open Quadtree
 
 (* Pour le moment, ce fichier contient les tests sur la combinaison ball-paddle-briques *)
 (* Ce fichier est notre jeu Arkanoid*)
 
 (*Module de configuration *)
 module Config = 
 struct
   
   let x = 300.0
   let y = 45.0
   let  width = 100.0
   let height = 10.0
   let brick_width = 45  
   let brick_height = 15 
   let rayon = 5.0  
   let augm = 10.
 
 end
 
 module type Params =
   sig
     include Frame
     val position0 : float * float
     val vitesse0  : float * float
     val briques0 : Brick.t list
     val quadtree0 : t quadtree
     val score : int

 end 
 
 module BallCaracter (Init : Params) =
   struct
     
     let dt2 = (Init.dt, Init.dt);;
 
     let (|+|) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2);;
     let (|*|) (x1, y1) (x2, y2) = (x1 *. x2, y1 *. y2);;
 
     let integre dt init flux =
       let rec acc =
         Tick (lazy (Some (init, Flux.map2 (fun a f -> a |+| (dt2 |*| f)) acc flux)))
       in acc;;
 
     let g = 9.81;;
 
     let rec position =
       Tick (lazy (Flux.uncons (
       integre dt2 Init.position0 vitesse          
         )))
 
     and vitesse =
       Tick (lazy (Flux.uncons (
       integre dt2 Init.vitesse0 acceleration
         )))
 
     and acceleration =
       Tick (lazy (Flux.uncons (
       Flux.constant (0., 0.)
         )))
     ;;
 
     let briques = 
       Tick (lazy (Flux.uncons (
       Flux.constant Init.briques0
         )))
     ;;

     let quadtree = 
      Tick (lazy (Flux.uncons (
      Flux.constant Init.quadtree0
        )));;

      let score = 
        Tick (lazy (Flux.uncons (
        Flux.constant Init.score
          )));;
     
      let etat = Flux.map2 (fun position (vitesse, briques, qt, score) ->
            (position, vitesse, briques, qt, score)) (position) 
            (Flux.map2 (fun vitesse (blocks, qt, score)-> (vitesse,blocks, qt, score)) (vitesse)  (Flux.map2 (fun blocks (qt, score) -> (blocks, qt, score)) (briques) ((Flux.map2 (fun qt score -> (qt, score))) (quadtree) (score))))

      
   end
 module Bouncing (F : Frame) = 
 struct
   module Balle = FreeFall(F)
   module MyPaddle = Paddle (F)
   let paddle = MyPaddle.create Config.x Config.y Config.width Config.height
   let collision_balle_paddle ((bx, by), _) (px, py, width, height) =
     bx >= px && bx <= px +. width && by >= py && by <= py +. height;;
 
   let rebond ((x, y), (dx, dy)) =
   (*  (rebond_x F.box_x x dx, rebondRaquette ((x,y),(dx,dy))) *)
   (rebond_x F.box_x x dx, rebond_y F.box_y y dy);;
 
   let rec bornesBrique rayon brique = let {x=rx;y=ry;width = rw;height = rh;level = rl;visible} = brique in ((rx-. rayon,rx+. rw +. rayon),(ry-. rayon, ry+.rh +.rayon));;
 
   
   let rec bornesBriques rayon briques = 
     let rec aux briques = 
       match briques with 
       | [] -> [];
       | {x=rx;y=ry;width = rw;height = rh;level = rl;visible}::q -> if (visible) then ((rx-. rayon,rx+.rw +. rayon),(ry-.rayon,ry+.rh +.rayon))::(aux q)
                             else aux q
     in aux briques;;
   
   let contact_brique_x ((infx,supx),(infy,supy)) (x,y) (dx,dy) m  = 
    false;;

   let contact_brique_y ((infx, supx), (infy, supy)) (x, y) (dx, dy) r =
    let result =
      (dy >= 0. && y <=infy && y>=infy-.15. && infx-.20.<=x && x<=supx+.20.)||
      (dy < 0. && y >= supy-.15. && y<= supy && infx-.20.<=x && x<=supx+.20.)
    in
    (*Printf.printf "infx: %f, infy: %f\n" infx infy;
    Printf.printf "supx: %f, supy: %f\n" supx supy;*)
    result;;
   
     
   
  let rebondBox ((x, y), (dx, dy)) =
     if y <= 0. then (300. , 300.) else
     let (rx,_) = mouse_pos () in 
    (rebond_x F.box_x x dx, if (contact_y (0., 480.) y dy) || ((y <= 53. && dy <= 0.) && ((x>=float_of_int rx -. 50.)&&(x<=float_of_int rx +. 50.))) then -. dy else dy)
 
   
   
  let rebondBrique ((x,y),(dx,dy),briques) =
      if y <= 0. then (300. , 300.) else 
        let bornes = bornesBriques 5.0 briques in
       (
         (if (List.fold_right (fun pair res -> res || (contact_brique_x pair (x,y) (dx,dy) 5. )) bornes false) then -.dx
           else dx
         ),
         
         (if (List.fold_right (fun pair res -> res || (contact_brique_y pair (x,y) (dx,dy) 5.) )) bornes false then -.dy 
           else dy)
       );;
   
 
   let contact ((x, y),(dx, dy)) = let (rx,_) = mouse_pos () in 
   contact_x F.box_x x dx || contact_y F.box_y y dy||((y <= 53. && dy <= 0.) && ((x>=float_of_int rx -. 50.)&&(x<=float_of_int rx +. 50.)))
    ;;
   let contactBriques ((x,y),(dx,dy),briques)= 
       let bornes = bornesBriques 5. briques in 
       List.fold_right (fun pair res -> res || (contact_brique_x pair (x,y) (dx,dy) 1.)) bornes false
       || List.fold_right (fun pair res -> res || (contact_brique_y pair (x,y) (dx,dy) 1.)) bornes false
       || contact ((x, y),(dx, dy));;
 
  
      
    let miseAjourScore ((x,y),(dx,dy),briques) score rayon =
          match briques with 
          | [] -> score - 1
          | l::q ->  let rec aux l score = 
                        match l with 
                        | [] -> score 
                        | t::q -> if (contact_brique_x (bornesBrique rayon t) (x,y) (dx,dy) 1. ) 
                                  || (contact_brique_y (bornesBrique rayon t) (x,y) (dx,dy) 1. ) then 
                                    match t with 
                                    | {x = briqX; y = briqY; width = briqWidth; height = briqHeight; visible = false; _}-> aux q (score+1)
                                  else aux q score
                      in aux briques score;;
        ;;

      
    let rec miseAjourBriques ((x, y), (dx, dy), briques) rayon =
          match briques with 
          | [] -> 
            briques
          | t::q -> 
            if contact_brique_x (bornesBrique rayon t) (x, y) (dx, dy) 1.
            || contact_brique_y (bornesBrique rayon t) (x, y) (dx, dy) 1. then
              begin
                
                updateVisibility ((x, y), (dx, dy), t) rayon @ miseAjourBriques ((x, y), (dx, dy), q) rayon
          
              end
            else 
              begin
                t :: miseAjourBriques ((x, y), (dx, dy), q) rayon
              end
         and updateVisibility ((x, y), (dx, dy), brick) rayon =
                { brick with visible = false } :: []
          ;;
        

   let gestionBornes flux contactBox contactBriques f1 f2 =
     Flux.unfold (fun (init, f) ->
         match Flux.uncons f with
               | None  -> None
               | Some (v, f') -> Some (v, 
                       match v with 
                       | ((x,y),(dx,dy),briques, quadtree, score)->
                           if init && contactBox ((x,y),(dx,dy)) then (false,f1 v)
                           else if init &&  contactBriques ((x,y),(dx,dy),briques) then (false,f2 v)
                           else (init,f'))    
     ) (true, flux);;
 

   let rec runGame (module Init : Params) =
    let module BwG = BallCaracter (Init) in
    Tick (lazy (Flux.uncons (
      gestionBornes BwG.etat contact contactBriques (fun  (((x, y), (dx, dy), briques, quadtree, score)) -> let module Restart =
                                            struct
                                              include Init
                                              let position0 = if y <= 0. then (0., 0.) else (x,y)
                                              let vitesse0 = rebondBox ((x,y),(dx,dy))
                                            end in
                                            runGame (module Restart))

                                          (fun ((x,y),(dx,dy),briques, quadtree, score) -> let module Restart =
                                            struct
                                              include Init
                                              let position0 = if y <= 0. then  (0., 0.) else (x,y)
                                              let vitesse0 = let (dx,dy) = rebondBrique ((x,y),(dx,dy),briques) in ( (if dx > 0. then dx+.Config.augm else dx-.Config.augm) , (if dy > 0. then dy+.Config.augm else dy-.Config.augm))
                                              let briques0 = miseAjourBriques ((x,y),(dx,dy),briques) 5.
                                              let score = miseAjourScore ((x,y),(dx,dy),briques0) score 5.
                                              let qt = create_quadtree (snd F.box_x) (snd F.box_y)
                                              let quadtree0 = List.fold_left MainBQB.insert_quadtree qt briques0
                                            end in
                                            runGame (module Restart))
                                          
  )));

 end
 
 
 
 (* Module de représentation graphique d'une balle-raquette-quadtree-score en 2D        *)
 (* la simulation s'obtient en appliquant draw à un flux d'états *)
 module Drawing (F : Frame) =
   struct
     include Config
     let current_paddle_position = ref (0., 0.)
     let draw (r : ((float * float) * (float * float) * t list * t quadtree * int) Flux.t) =
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
           | Some (((x, y), (dx, dy), brick, qt, score), r') ->
             
             begin
               Graphics.clear_graph ();
               let module MyPaddle = Paddle (F) in
               let paddle = MyPaddle.create x Config.y width height in
               set_color black;
               moveto 10 10; 
               draw_string ("Final Score: " ^ string_of_int score);

               Graphics.draw_circle (int_of_float x) (int_of_float y) 5;
               Graphics.fill_circle (int_of_float x) (int_of_float y) 5;
               (*Printf.printf "Position de la balle : (%.2f, %.2f)\n" x y;*)
               (*moveto (int_of_float x) (int_of_float y);
               draw_string (Printf.sprintf "(%.2f, %.2f)" x y);*)
               MyPaddle.update_with_mouse paddle;
               current_paddle_position := (MyPaddle.get_x paddle, MyPaddle.get_y paddle);
               MyPaddle.draw_paddle paddle;
               MainBQB.draw_quadtree qt;
               
              (*Printf.printf "Position de la raquette : (%.2f, %.2f)\n" (fst !current_paddle_position) (snd !current_paddle_position);
                *)  
               (*moveto (int_of_float (fst !current_paddle_position)) (int_of_float (snd !current_paddle_position));
               draw_string (Printf.sprintf "(%.2f, %.2f)" (fst !current_paddle_position) (snd !current_paddle_position));*)
               Graphics.synchronize ();
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
 
 
 module F1: Frame = 
 struct
   let dt = 0.1
   let box_x = (0., 800.0)
   let box_y = (0., 600.0)
   
 end
 
 
 module Initialisation = 
 struct 
   let dt = 0.1
   let box_x = (0., 800.0)
   let box_y = (0., 600.0)
   let position0 = (0., 0.)
   let vitesse0 = (300., 300.)
   let briques0 = MainBQB.create_bricks ()
   let quadtree0 = MainBQB.createQ (snd box_x) (snd box_y)
   let score = 0
   let tentatives = 3

 end
 
 let main () = 
 
   let etat0 = ((0.0, 0.0), (400.0, 400.0)) in
   let module D = Drawing (F1) in
   let module FB = Bouncing (F1) in
   let briques = MainBQB.create_bricks () in
   D.draw(FB.runGame (module Initialisation));


