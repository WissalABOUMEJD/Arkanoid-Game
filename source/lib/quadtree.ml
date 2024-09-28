open Graphics
open Brick
 (* Ce fichier est utilisé pour avoir les types définis (quadtree, rect, vec, quadnode) pour notre jeu *)
 (* les fonctions définies dans ce fichier étaient notre première version de quadtree pour insertion d'un objet bidimensionnel e pour nlimit défini*)
    

(********************************************************************************************)

(* Définition du type pour un point *)
type vec = { x : float ; y : float }

(* Création d'un rectangle *)
(* la région de la feuille *)
type rect = {
  ul : vec ; (* coin supérieur gauche *)
  lr : vec   (* coin inférieur droit *)
}

type quadrant =
| NW
| NE
| SW
| SE

(* Vérifie si un point est dans une région *)
let point_inside_region point rect =
  point.x >= rect.ul.x &&
    point.x <= rect.lr.x &&
    point.y >= rect.ul.y &&
    point.y <= rect.lr.y

(* Calcule le centre d'une region *)
let centre_region rect = { x = (rect.ul.x +. rect.lr.x) /. 2. ;
		    y = (rect.ul.y +. rect.lr.y) /. 2. }

(* Définition du type pour un quadtree *)
type 'a quadtree =
  | Leaf of 'a list * rect
  | Node of 'a quadnode

(* Définition du type pour un quadnode *)
and 'a quadnode = {
  centre : vec ; (* centre du rectangle associé à ce noeud *)
  region : rect ;  (* la Grande région ou se trouve le noeud *)
  nw : 'a quadtree ; 
  ne : 'a quadtree ;
  sw : 'a quadtree ;
  se : 'a quadtree ;
}

(* Retourne le rectangle d'un noeud quadtree *)
let region = function
  | Leaf (_,region) -> region
  | Node n -> n.region;;

(* Vérifie si deux régions se chevauchent *)
let superposition_regions r s =
  ( r.ul.x < s.lr.x && r.lr.x > s.ul.x )
  && ( r.ul.y < s.lr.y && r.lr.y > s.ul.y );;
    (* true quand les rectangles r et s se recouvrent *)

(* Création d'un quadtree vide avec région donnée*)
let empty v = Leaf ([],v);;

(* Insère un objet e dans un quadtree *)
let rec insert nlimit e = function
  | Leaf (values, region) ->
    if List.length values + 1 > nlimit 
      then
	let c = centre_region region in
	let n = Node 
	  { centre = c ;
            region = region ;
            nw = empty { ul = region.ul ; lr = c } ;
            sw = empty { ul = { x = region.ul.x ; y = c.y } ; lr = { x = c.x ; y = region.lr.y } };
            se = empty { ul = c ; lr = region.lr } ;
            ne = empty { ul = { x = c.x ; y = region.ul.y } ; lr = { x = region.lr.x ; y = c.y } } }
	in List.fold_right (insert nlimit) values (insert nlimit e n)
      else Leaf (e :: values, region)
  | Node n ->
      let pos = e in
      let node =
	match (pos.x < n.centre.x, pos.y < n.centre.y) with
	    (true, true) -> { n with nw = insert nlimit e n.nw }
	  | (true, false) -> { n with sw = insert nlimit e n.sw }
	  | (false, false) -> { n with se = insert nlimit e n.se }
	  | (false, true) -> { n with ne = insert nlimit e n.ne }
      in Node node;;


let make_leaf () = Leaf ([], { ul = { x = 0.0; y = 0.0 }; lr = { x = 0.0; y = 0.0 } })

