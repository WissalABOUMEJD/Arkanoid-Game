open Graphics
open Iterator

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
    set_color red;
    fill_rect (int_of_float paddle.x) (int_of_float paddle.y) (int_of_float paddle.width) (int_of_float paddle.height)
end
