(* let game_hello () = print_endline "Hello, Newtonoiders!"
 *)
open Graphics
open Iterator
open Ball
open Paddle

module Config = 
struct
  
  let x = 300.0
  let y = 45.0
  let  width = 100.0
  let height = 10.0
   
end

(* Module de représentation graphique d'une balle en 2D         *)
(* la simulation s'obtient en appliquant draw à un flux d'états *)
module Drawing (F : Frame) =
  struct
    include Config
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
              Graphics.clear_graph ();
              let module MyPaddle = Paddle (F) in
              let paddle = MyPaddle.create Config.x Config.y width height in
              set_color black;
              Graphics.draw_circle (int_of_float x) (int_of_float y) 5;
              Graphics.fill_circle (int_of_float x) (int_of_float y) 5;
              MyPaddle.update_with_mouse paddle;
              MyPaddle.draw_paddle paddle; 
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


(*Main*)
module F1: Frame = 
struct
  let dt = 0.1
  let box_x = (0., 640.0)
  let box_y = (0., 480.0)
end

let etat0 = ((300.0, 46.0), (40.0, 200.0))
module D = Drawing (F1)
module FB = Bouncing (F1)
let main () = D.draw(FB.run etat0)