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
  ( j >= (-dim+1) / 2) && ( j <= (dim+1)/2 ) && ( k >= -dim ) && ( k <= dim ) ;;

let est_dans_etoile = fun (a:case) ->
  let (i, j, k) = a in
  i <= (2*dim) && i >= (-2*dim) && j <= (2*dim) && j >= (-2*dim) && k <= (2*dim) && k >= (-2*dim) ;;


let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ])

let liste_joueurs (_, l) = l

let quelle_couleur = fun (a:case) (config:configuration) ->
  if est_dans_losange a then Libre else Dehors ;;

type coup = Du of case * case | Sm of case list

let mis_a_jour_configuration _ _ = Error "To do"

let gagnant _ = Libre
