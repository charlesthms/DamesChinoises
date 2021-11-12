let dim = 3

type case = int * int * int

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors
  (*case en dehors du plateau, utile pour l'affichage*)
  | Nombre of int
  (*pour mettre des petits noms*)
  | Nom of string

let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"
  | Nombre n -> string_of_int n
  (*pour mettre des petits noms*)
  | Nom s -> s

type case_coloree = case * couleur

type configuration = case_coloree list * couleur list

let (te:case) = (-4, 8, 1) ;;
let (test:case_coloree) = (te, Marron) ;;
let configuration_initial = (([test], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ]):configuration)
let liste_joueurs (_, l) = l

let est_dans_losange = fun (a:case) ->
  let (i, j, k) = a in 
  j >= -dim && j <= dim && k >= -dim && k <= dim ;;

let est_dans_etoile = fun (a:case) ->
  let (i, j, k) = a in
  (est_dans_losange a || (j >= -dim && j <= dim && i >= -dim && i <= dim || k >= -dim && k <= dim && i >= -dim && i <= dim)) && i+j+k=0 ;;

let quelle_couleur = fun (a:case) (config:configuration) ->
  if est_dans_etoile a then Libre else Dehors;;


type coup = Du of case * case | Sm of case list
let mis_a_jour_configuration _ _ = Error "To do"
let gagnant _ = Libre

let rec tourne_case n (case:case) = let (i, j, k) = case in
  if n=1 then ((i+j, j+k, k+i):case) else tourne_case (n-1) (i+j, j+k, k+i) ;;

let rec case_modif lst = match lst with
  [] -> []
  | h::t -> let (c, y) = (h:case_coloree) in ((tourne_case 1 c, y):case_coloree) :: case_modif t ;;
let tourne_config (config:configuration) = 
  let (cc, x) = config in ((case_modif cc,  x):configuration) ;;

(* TEST *)
let cfg = [ (((6, -3, -3), Vert):case_coloree); (((5, -3, -3), Vert):case_coloree); (((4, -3, -3), Vert):case_coloree) ] ;;
case_modif cfg ;;

let sont_cases_voisines (c1:case) (c2:case) = let (i1, j1, k1) = c1 in let (i2, j2, k2) = c2 in
  i2 = i1 + 1 || j2 = j1 +1 || k2 = k1 + 1  ;;

(* TEST *)
let (c1:case) = (-4, 3, 1) ;;
let (c2:case) = (-5, 3, 2) ;;

sont_cases_voisines c1 c2 ;;

let rec case_dans_config (c:case) (cfg:configuration) = let (cc, couleur) = cfg in match cc with
  [] -> false
  | h :: t -> 
    let (case, color) = h in 
    let (i, j, k) = case in
    let (x, y, z) = c in
    i=x && j=y && k=z && not (color = Libre) || case_dans_config c (t, couleur) ;;

case_dans_config te configuration_initial ;;

let rec quelle_couleur (c:case) (cfg:configuration) = let (cc, couleur) = cfg in match cc with
  [] -> Dehors
  | h :: t -> 
    let (case, color) = h in
    let (x, y, z) = case in
    let (i, j, k) = c in
    if i=x && j=y && k=z && (case_dans_config c cfg) then color else quelle_couleur c (t, couleur) ;;

quelle_couleur te configuration_initial ;;
