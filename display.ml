open Html

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

  val coup_possible : configuration -> case -> (coup * case) list

  val gagnant : configuration -> couleur
end

module Play (DC : CONF) = struct
  open DC

  let divneg n m =
    if n > 0 then n / m else if n mod m = 0 then n / m else (n / m) - 1

  let transforme n m =
    ((2 * dim) - n, divneg (n + m - (5 * dim)) 2, divneg (n - m + dim) 2)

  let string_of_case (a1, a2, a3) =
    "(" ^ string_of_int a1 ^ ", " ^ string_of_int a2 ^ ", " ^ string_of_int a3
    ^ ")"

  let rec string_of_coup = function
    | Du (a1, a2) ->
        "Du(" ^ (string_of_case a1 ^ ", " ^ string_of_case a2) ^ ")"
    | Sm a1 ->
        "Sm("
        ^ (fun s ->
            List.fold_left
              (fun a b ->
                (if a <> "[" then a ^ "; " else "[") ^ string_of_case b)
              "[" s
            ^ "]")
            a1
        ^ ")"

  let couleur2span sp c =
    let s = string_of_couleur c in
    match c with
    | Libre -> sp "pionLibre" " "
    | Dehors -> []
    | Nombre i -> sp "" (string_of_int i)
    | Nom s -> sp "" s
    | _ -> sp ("pion" ^ s) (Char.escaped (String.get s 0))

  let to_coup = function
    | [] | [ _ ] -> Error "Selectionne au moins deux cases"
    | [ ((a, b, c) as t1); ((a', b', c') as t2) ]
      when (a - a' = b' - b && abs (a - a') = 1 && c = c')
           || (a - a' = c' - c && abs (a - a') = 1 && b = b')
           || (b - b' = c' - c && abs (c - c') = 1 && a = a') ->
        Ok (Du (t2, t1))
    | l -> Ok (Sm (List.rev l))

  let col_of_list i l =
    let n = List.length l in
    if i * n mod 6 = 0 then List.nth l (i * n / 6) else Libre

  let cliquable_case sel f c =
    td
    @@ couleur2span
         (fun cl content ->
           [
             button
               ~class_:("case " ^ sel ^ cl)
               ~on_click:(fun () -> f ())
               [ text content ];
           ])
         c

  let rec affiche_ligne sel f n m config =
    if m = (6 * dim) + 2 then []
    else
      let c = transforme n m in
      let jeton = cliquable_case (sel c) (f c) (quelle_couleur c config) in
      jeton :: affiche_ligne sel f n (m + 1) config

  let affiche_liste sel f config =
    let rec affiche_aux n config =
      if n = (4 * dim) + 1 then []
      else tr (affiche_ligne sel f n 0 config) :: affiche_aux (n + 1) config
    in
    affiche_aux 0 config

  let affiche update click conf selection =
    let lc = liste_joueurs conf in

    let casep =
      match selection with [] -> [] | c1 :: _ -> DC.coup_possible conf c1
    in
    let tp =
      table ~id:"plateau"
        (affiche_liste
           (fun c ->
             match
               (List.mem c selection, List.exists (fun (_, c2) -> c2 = c) casep)
             with
             | true, false -> "selected "
             | false, true -> "possible "
             | true, true -> "selectpos "
             | _ -> " ")
           (fun c () ->
             match selection with
             | c2 :: q when c = c2 -> click conf selection ()
             | _ -> update conf (c :: selection))
           conf)
    in
    let marqueur i =
      couleur2span
        (fun cl content -> [ span ~class_:(cl ^ " marqeur") [ text content ] ])
        (col_of_list i lc)
    in

    table
      [
        tr [ td []; td (marqueur 3); td [] ];
        tr [ td (marqueur 2); td ~rowspan:2 [ tp ]; td (marqueur 4) ];
        tr [ td (marqueur 1); td (marqueur 5) ];
        tr [ td []; td (marqueur 0); td [] ];
      ]

  let joueur, pjoueur = div' []

  let plateau, pup = div' []

  let buttondiv, buttondiv_up = div' []

  let console, console_up = text_area' ~class_:"console" ~is_read_only:true ""

  let error, error_up = text_area' ~class_:"consoleerr" ~is_read_only:true ""

  let log_coup, log_coup_up =
    div' ~class_:"console" [ text "Liste des coups de la partie"; br () ]

  let send_coup update conf select () =
    match to_coup select with
    | Ok c -> (
        match mis_a_jour_configuration conf c with
        | Ok x ->
            update x [];
            let col = List.hd (liste_joueurs conf) in
            let cp =
              couleur2span
                (fun cl _ ->
                  [
                    span ~class_:cl [ text (string_of_coup c) ]; text ","; br ();
                  ])
                col
            in
            log_coup_up ~append:true cp;
            error_up ""
        | Error err ->
            update conf [];
            error_up err)
    | Error err ->
        update conf [];
        error_up err

  let rec cup conf selection =
    let s =
      List.fold_left
        (fun acc c -> string_of_case c ^ (if acc <> "" then "; " else "") ^ acc)
        "" selection
    in
    let cj = List.hd @@ liste_joueurs conf in
    pjoueur [ text ("Le tour est au joueur " ^ string_of_couleur cj) ];
    pup [ affiche cup (send_coup cup) conf selection ];
    buttondiv_up
      [
        button ~on_click:(send_coup cup conf selection) [ text "Jouer" ];
        button ~on_click:(fun () -> cup conf []) [ text "Reset" ];
      ];
    console_up ("[" ^ s ^ "]")

  let _ =
    run (fun () ->
        cup configuration_initial [];
        div ~class_:"main"
          [
            plateau;
            div [ joueur; br (); buttondiv; br (); console; error; log_coup ];
          ])
end

module P = Play (Rendu_etudiant)
open P
