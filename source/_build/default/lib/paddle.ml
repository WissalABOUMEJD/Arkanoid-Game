open Graphics

module type Frame = sig
  val window_width : float
  val window_height : float
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
      max 0.0 (min (F.window_width -. paddle.width) (float_of_int mouse_x -. paddle_width_half))
    in
    move_to paddle new_x

  let draw_paddle paddle =
    set_color green;
    fill_rect (int_of_float paddle.x) (int_of_float paddle.y) (int_of_float paddle.width) (int_of_float paddle.height)
end

(* Main *)
module F1 : Frame = struct
  let window_width = 800.0
  let window_height = 600.0
end

let main () =
  open_graph (Format.sprintf " %dx%d" (int_of_float F1.window_width) (int_of_float F1.window_height));
  auto_synchronize false;

  let module MyPaddle = Paddle (F1) in
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
