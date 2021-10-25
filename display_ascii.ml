module type CONF = sig
  val dim : int

  type case = int * int * int

  type couleur =
    | Vert
    | Jaune
    | Rouge
    | Noir
    | Bleu
    | Marron
    (*case libre du plateau*)
    | Libre
    (*case en dehors du plateau, utile pour l'affichage*)
    | Dehors
    | Nombre of int (*pour mettre des nombre dans une configuration*)
    | Nom of string
  (*pour mettre des petits noms*)

  val string_of_couleur : couleur -> string

  type configuration

  val liste_joueurs : configuration -> couleur list

  val configuration_initial : configuration

  val quelle_couleur : case -> configuration -> couleur

  type coup = Du of case * case | Sm of case list

  val mis_a_jour_configuration :
    configuration -> coup -> (configuration, string) result

  val gagnant : configuration -> couleur
end

module Play (DC : CONF) = struct
  open DC

  let divneg n m =
    if n > 0 then n / m else if n mod m = 0 then n / m else (n / m) - 1

  let transforme n m =
    ((2 * dim) - n, divneg (n + m - (5 * dim)) 2, divneg (n - m + dim) 2)

  let couleur2string coul =
    match coul with
    | Vert -> " V "
    | Jaune -> " J "
    | Rouge -> " R "
    | Noir -> " N "
    | Bleu -> " B "
    | Marron -> " M "
    | Libre -> " . "
    | Dehors -> "   "
    | Nombre x -> string_of_int x
    | Nom x -> x

  (*case du haut*)

  let rec affiche_ligne n m config =
    if m = (8 * dim) + 2 then " "
    else
      let c = transforme n m in
      couleur2string (quelle_couleur c config) ^ affiche_ligne n (m + 1) config

  let affiche_liste config =
    let rec affiche_aux n config =
      if n = (4 * dim) + 1 then []
      else affiche_ligne n 0 config :: affiche_aux (n + 1) config
    in
    affiche_aux 0 config

  let affiche config =
    let affiche_aux stringlist =
      List.fold_left
        (fun accu e ->
          print_endline e;
          print_endline "\n")
        () stringlist
    in
    affiche_aux (affiche_liste config)

  let to_coup = function
    | [] | [ _ ] -> Error "Selectionne au moins deux cases"
    | [ t1; t2 ] -> Ok (Du (t1, t2))
    | l -> Ok (Sm l)

  let rec parse_coup s =
    match String.index_opt s '(' with
    | None -> Some []
    | Some si -> (
        match String.index_from_opt s si ')' with
        | None -> None
        | Some sf -> (
            let case = String.sub s si (sf - si + 1) in
            let pcase =
              Scanf.sscanf case "( %i , %i , %i )" (fun i j k -> (i, j, k))
            in
            match parse_coup (String.sub s sf (String.length s - sf)) with
            | Some l -> Some (pcase :: l)
            | None -> None))

  let rec run c =
    affiche c;
    match parse_coup (read_line ()) with
    | None ->
        print_endline "Entrée non valide";
        run c
    | Some l -> (
        match to_coup l with
        | Ok cp -> (
            match mis_a_jour_configuration c cp with
            | Ok c2 -> run c2
            | Error e ->
                print_endline ("Erreur:" ^ e);
                run c)
        | Error e ->
            print_endline ("Erreur:" ^ e);
            run c)

  let _ = run configuration_initial
end
;;

#mod_use "rendu_etudiant.ml"

open Play (Rendu_etudiant)
