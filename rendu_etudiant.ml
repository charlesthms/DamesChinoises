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

let liste_joueurs (_, l) = l

let est_dans_losange = fun (a:case) ->
  let (i, j, k) = a in 
  j >= -dim && j <= dim && k >= -dim && k <= dim ;;

let est_dans_etoile = fun (a:case) -> (* Union des 3 losanges *)
  let (i, j, k) = a in
  (est_dans_losange a || (j >= -dim && j <= dim && i >= -dim && i <= dim || k >= -dim && k <= dim && i >= -dim && i <= dim)) && i+j+k=0 ;;


let rec tourne_case n (case:case) = let (i, j, k) = case in
  if n=1 then ((i+j, j+k, k+i):case) else tourne_case (n-1) (i+j, j+k, k+i) ;;

let tourne_config (config:configuration) = 
  let rec tourne_config_aux lst = match lst with
      [] -> []
    | h::t -> let (c, y) = (h:case_coloree) in ((tourne_case 1 c, y):case_coloree) :: tourne_config_aux t
  in
  let (cc, x) = config in ((tourne_config_aux cc,  x):configuration) ;;

let sont_cases_voisines (c1:case) (c2:case) = let (i1, j1, k1) = c1 in let (i2, j2, k2) = c2 in
  i2 = i1 + 1 || j2 = j1 +1 || k2 = k1 + 1  ;;

let rec case_dans_config (c:case) (cfg:configuration) = let (cc, couleur) = cfg in match cc with
    [] -> false
  | h :: t -> 
    let (case, color) = h in 
    let (i, j, k) = case in
    let (x, y, z) = c in
    i=x && j=y && k=z || case_dans_config c (t, couleur) ;;


(*let quelle_couleur = fun (c:case) (cfg:configuration) ->
   if est_dans_etoile c then Libre else Dehors;;*)

let rec quelle_couleur (c:case) (cfg:configuration) = let (cc, couleurs) = cfg in match cc with
    [] -> if est_dans_etoile c then Libre else Dehors
  | h :: t -> 
    let (case_local, color) = h in
    let (x, y, z) = case_local in
    let (i, j, k) = c in

    if x=i && y=j && z=k then color else quelle_couleur c (t, couleurs) ;;

let (test:case) = (-6, 3, 3) ;;
let (cc:case_coloree) = (test, Vert);;


let remplir_triangle (cfg:configuration) color (case:case) = 
  let rec remplir_ligne (cfg:configuration) (case:case) color arret = let (cc, colors) = cfg in
    if arret=0 then cfg else
      let (i, j, k) = case in
      let rec add_diag cc (case:case) color lim =
        if lim=0 then cc
        else 
          let (i,j,k) = case in if est_dans_etoile case then ((i,j,k), color) :: add_diag cc (i+1,j-1,k) color (lim-1) 
          else add_diag cc (i+1,j,k) color (lim-1)
      in
      remplir_ligne (add_diag cc case color (arret), colors) ((i+1, j, k-1):case) color (arret - 1) 
  in 
  remplir_ligne cfg case color dim  ;;


let rec remplir_init_aux lst cfg = match lst with
    [] -> cfg
  | h::t -> 
    let sub lst color cfg = 
      tourne_config (remplir_triangle cfg color (-2*dim, dim, dim))
    in remplir_init_aux t (sub lst h cfg) ;;

let remplir_init lst = let cfg = ([], lst) in remplir_init_aux lst cfg ;;


let configuration_initial =
  remplir_init [ Vert ; Jaune ; Rouge ; Noir ; Bleu ; Marron ] ;;

let rec est_dep_unit_aux (cfg:configuration) (c1:case) = let (cc,col) = cfg in match col with
    [] -> false
  | a::b -> a = quelle_couleur c1 cfg ;;

let est_dep_unit (cfg:configuration) (c1:case) (c2:case) = 
  sont_cases_voisines c1 c2 && est_dep_unit_aux cfg c1 && quelle_couleur c2 cfg = Libre && est_dans_losange c2;;


let rec fait_dep_unit_aux (cfg:configuration) (c1:case) (c2:case) = let (cc,col) = cfg in match cc with
    [] -> []
  | h :: t ->
    let (case_local, color) = h in
    let (x, y, z) = case_local in
    let (i, j, k) = c1 in
    let (a,b,c) = c2 in

    if x=i && y=j && z=k then quelle_couleur c2 cfg :: fait_dep_unit_aux (t,col) c1 c2 
    else if x=a && y=b && z=c then quelle_couleur c1 cfg :: fait_dep_unit_aux (t,col) c1 c2 
    else fait_dep_unit_aux (t,col) c1 c2 ;;

let fait_dep_unit_aux (cfg:configuration) (c1:case) (c2:case) = let (cc, col) = cfg in (fait_dep_unit_aux cfg c1 c2, col);;

type coup = 
  | Du of case * case 
  | Sm of case list
let mis_a_jour_configuration cfg coup = Error "To do"
let gagnant _ = Libre


let rot lst =
  let rec iterate [] acc = match [] with
    [] -> []
    | [x] -> x :: List.rev acc
    | x :: lst -> iterate lst (x :: acc) 
  in
  iterate [] lst;;

rot [ Vert ; Jaune ; Rouge ; Noir ; Bleu ; Marron ] ;;