open Graphics

module type Frame =
  sig
    val dt : float
    val box_x : float * float
    val box_y : float * float
  end

module type PaddleI = sig
  type t

  val create : float -> float -> float -> float -> t
  val move_to : t -> float -> unit
  val get_position : t -> float
  val update_with_mouse : t -> unit
  val draw_paddle : t -> unit 
end

module Paddle (F : Frame) : PaddleI = struct
  type t = {
    mutable x : float;
    mutable y : float;
    width : float;
    height : float;
  }

  let create x y width height = { x; y; width; height }

  let move_to paddle new_x =
    paddle.x <- new_x

  let get_position paddle =
    paddle.x

  let update_with_mouse paddle =
    let mouse_x, _ = Graphics.mouse_pos () in
    let paddle_width_half = paddle.width /. 2.0 in
    let new_x =
      max (fst F.box_x) (min (snd F.box_x -. paddle.width) (float_of_int mouse_x -. paddle_width_half))
    in
    move_to paddle new_x

  let draw_paddle paddle =
    set_color green;
    fill_rect (int_of_float paddle.x) (int_of_float paddle.y) (int_of_float paddle.width) (int_of_float paddle.height)
end

(* Main *)
module F: Frame = 
struct
  let dt = 0.0
  let box_x = (0., 640.0)
  let box_y = (0., 480.0)
end

let main () =
  let (inf_x, sup_x) = F.box_x in
  let (inf_y, sup_y) = F.box_y in
  let size_x = int_of_float (sup_x -. inf_x) in
  let size_y = int_of_float (sup_y -. inf_y) in
  Graphics.open_graph (Format.sprintf " %dx%d" size_x size_y);
  auto_synchronize false;

  let module MyPaddle = Paddle (F) in
  let paddle = MyPaddle.create 300.0 45.0 100.0 10.0 in

  let rec game_loop () =
    clear_graph ();
    MyPaddle.update_with_mouse paddle;
    MyPaddle.draw_paddle paddle;  (* Appel à la fonction draw_paddle *)
    synchronize ();
    Unix.sleepf 0.01;
    game_loop ()
  in

  game_loop ()
