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
 
let est_dans_losange = fun (a:case) ->
  let (i, j, k) = a in 
  j >= -dim && j <= dim && k >= -dim && k <= dim ;;

let est_dans_etoile = fun (a:case) ->
  let (i, j, k) = a in
  (est_dans_losange a || (j >= -dim && j <= dim && i >= -dim && i <= dim || k >= -dim && k <= dim && i >= -dim && i <= dim)) && i+j+k=0 ;;

let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ])

let liste_joueurs (_, l) = l

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

let cfg = [ (((6, -3, -3), Vert):case_coloree); (((5, -3, -3), Vert):case_coloree); (((4, -3, -3), Vert):case_coloree) ] ;;
case_modif cfg ;;