open Graphics
open Quadtree


module type BrickInterf =
  sig
    type t = { x : float; y : float; width : float; height : float; level : int }
    val create : float -> float -> float -> float -> int -> t
    val draw : t -> unit
  end 

module Brick : BrickInterf = 
struct
  type t = { x : float; y : float; width : float; height : float; level : int }
  let create x y width height level = { x; y; width; height; level }

  (*let draw brick color=
    set_color color;
    fill_rect (int_of_float brick.x) (int_of_float brick.y) (int_of_float brick.width) (int_of_float brick.height)*)

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
end


include Brick
let brick_width = 45
let brick_height = 10

(* Create a simple quadtree for demonstration purposes *)
let create_quadtree_demo bricks frame_width frame_height ball_radius =
  let quadtree = Quadtree.create_quadtree frame_width frame_height ball_radius in
  print_string "Hello, tree \n";
  List.fold_left (fun qt brick ->
    let brick_region = { Quadtree.ul = { x = 0.0; y = 0.0 }; Quadtree.lr = { x = frame_width /. 2.0 ; y = frame_height /. 2.0 } } in
    Quadtree.insert_brick_into_region brick brick_region qt
  ) quadtree bricks

(* Draw the quadtree by recursively traversing it *)
(*
let rec draw_quadtree qt =
  match qt with
  | Quadtree.Leaf (bricks, _) -> List.iter Brick.draw bricks
  | Quadtree.Node { nw; ne; sw; se; _ } ->
      draw_quadtree nw;
      draw_quadtree ne;
      draw_quadtree sw;
      draw_quadtree se;;
*)
(* Draw the quadtree by recursively traversing it *)
let draw_quadtree quadtree =
        let rec draw_node node =
          print_string "Hello, node \n";
          match node with
          | Leaf (_, region) -> draw_region region
          | Node n ->
              draw_region n.region;
              draw_node n.nw;
              draw_node n.ne;
              draw_node n.sw;
              draw_node n.se
        and draw_region region =
          print_string "Hello, region \n";
          set_color Graphics.blue; (* Choisissez la couleur de votre choix *)
          fill_rect
            (int_of_float region.ul.x)
            (int_of_float region.ul.y)
            (int_of_float (region.lr.x -. region.ul.x))
            (int_of_float (region.lr.y -. region.ul.y))
        in
        draw_node quadtree
 
(*

let main () =
  open_graph " 800x600";
  auto_synchronize false; (* Disable automatic synchronization for faster drawing *)

  let brick1 = Brick.create 50.0 50.0 (float brick_width) (float brick_height) 1 in
  let brick2 = Brick.create 200.0 100.0 (float brick_width) (float brick_height) 2 in
  let brick3 = Brick.create 400.0 300.0 (float brick_width) (float brick_height) 3 in

  let bricks = [brick1; brick2; brick3 ] in
  (*List.iter Brick.draw bricks; (* Draw the bricks *)*)

  let frame_width = 800.0 in
  let frame_height = 600.0 in
  let ball_radius = 20 in

  (* Create and draw the quadtree *)
  let quadtree = create_quadtree_demo bricks frame_width frame_height ball_radius in
  draw_quadtree quadtree;

  synchronize (); (* Synchronize the drawing *)
  ignore (wait_next_event [Key_pressed]); (* Wait for a key press before closing *)

  close_graph ()

let () = main ()


(*
let () =
  open_graph " 800x600";
  let frame_width = 800.0 in
  let frame_height = 600.0 in
  let ball_radius = 20 in
  let quadtree = create_quadtree frame_width frame_height ball_radius in
  draw_quadtree quadtree;
  Unix.sleep 2;  (* Pause pendant 2 secondes avant de fermer la fenêtre *)
  close_graph ()
*)


*)

(* Génère un tableau de points 2D aléatoires dans une plage spécifiée*)
(*
let empty_quadtree = Leaf ([], { ul = { x = 0.0; y = 0.0 }; lr = { x = 0.0; y = 0.0 } })

let rec insertNew value position quadtree =
  let rec subdivide region =
    let centre = { x = (region.ul.x +. region.lr.x) /. 2.0; y = (region.ul.y +. region.lr.y) /. 2.0 } in
    let ul = region.ul in
    let lr = region.lr in
    {
      centre;
      region;
      nw = Leaf ([], { ul = { x = ul.x; y = ul.y }; lr = centre });
      ne = Leaf ([], { ul = { x = centre.x; y = ul.y }; lr = { x = lr.x; y = centre.y } });
      sw = Leaf ([], { ul = { x = ul.x; y = centre.y }; lr = { x = centre.x; y = lr.y } });
      se = Leaf ([], { ul = centre; lr = lr });
    }
  in
  let rec insert_value value position quadtree =
    match quadtree with
    | Leaf (values, region) ->
        let new_values = (value, position) :: values in
        if List.length new_values <= 10 then
          Leaf (new_values, region)
        else
          let divided_node = subdivide region in
          List.fold_left (fun qt (v, pos) -> insert_value v pos qt) (Node divided_node) new_values
    | Node node ->
        let quadrant = determine_quadrant position node.centre in
        let subquadtree =
          match quadrant with
          | NW -> node.nw
          | NE -> node.ne
          | SW -> node.sw
          | SE -> node.se
        in
        let updated_subquadtree = insert_value value position subquadtree in
        match quadrant with
        | NW -> Node { node with nw = updated_subquadtree }
        | NE -> Node { node with ne = updated_subquadtree }
        | SW -> Node { node with sw = updated_subquadtree }
        | SE -> Node { node with se = updated_subquadtree }
  in
  insert_value value position quadtree

and determine_quadrant position centre =
  if position.x <= centre.x then
    if position.y <= centre.y then NW else SW
  else
    if position.y <= centre.y then NE else SE

let rec draw_quadtree quadtree =
  let rec draw_node node =
    match node with
    | Leaf (_, region) -> draw_region region
    | Node n ->
        draw_region n.region;
        draw_node n.nw;
        draw_node n.ne;
        draw_node n.sw;
        draw_node n.se
  and draw_region region =
    Graphics.set_color Graphics.blue;
    Graphics.fill_rect
      (int_of_float region.ul.x)
      (int_of_float region.ul.y)
      (int_of_float (region.lr.x -. region.ul.x))
      (int_of_float (region.lr.y -. region.ul.y))
  in
  draw_node quadtree

let random_quadtree () =
  let qt =
    insertNew 10 (1.0, 1.0)
      (insertNew 10 (1.0, 2.0)
         (insertNew 10 (5.0, 3.0)
            (insertNew 10 (3.0, 4.0)
               (insertNew 10 (7.0, 5.0) empty_quadtree))))
  in
  draw_quadtree qt;
  ignore (wait_next_event [Key_pressed]); (* Wait for a key press before closing *)
  Graphics.close_graph ()

let () =
  Graphics.open_graph " 800x600";
  random_quadtree ()



*)
