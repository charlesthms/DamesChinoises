(******************************************************************************)
(* Copyright (c) 2016 DooMeeR                                                 *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining      *)
(* a copy of this software and associated documentation files (the            *)
(* "Software"), to deal in the Software without restriction, including        *)
(* without limitation the rights to use, copy, modify, merge, publish,        *)
(* distribute, sublicense, and/or sell copies of the Software, and to         *)
(* permit persons to whom the Software is furnished to do so, subject to      *)
(* the following conditions:                                                  *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be             *)
(* included in all copies or substantial portions of the Software.            *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,            *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF         *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                      *)
(* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE     *)
(* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION     *)
(* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION      *)
(* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.            *)
(******************************************************************************)

(* Modification by Benoit Barbot *)

open Js_of_ocaml

type t = Dom.node Js.t

let alert x = Dom_html.window##alert (Js.string x)

let br () =
  let br = Dom_html.document##createElement (Js.string "BR") in
  (br :> t)

let text value =
  let text = Dom_html.document##createTextNode (Js.string value) in
  (text :> t)

let text' value =
  let text = Dom_html.document##createTextNode (Js.string value) in
  let set_text value = text##replaceData 0 text##.length (Js.string value) in
  ((text :> t), set_text)

let sc v class_ = Option.iter (fun x -> v##.className := Js.string x) class_

let si v id = Option.iter (fun x -> v##.id := Js.string x) id

let append_node parent node =
  let (_ : Dom.node Js.t) = parent##appendChild node in
  ()

let img ?class_ ?id ?alt src =
  let alt = match alt with None -> src | Some alt -> alt in
  let img = Dom_html.(createImg document) in
  img##.src := Js.string src;
  img##.alt := Js.string alt;
  sc img class_;
  si img id;
  (img :> t)

let canvas ?class_ ?id () =
  let ca = Dom_html.(createCanvas document) in
  (*ca##.width := 2048;
    ca##.height := 1024;*)
  sc ca class_;
  si ca id;
  (ca :> t)

let a ?class_ ?id ?href ?on_click items =
  let a = Dom_html.(createA document) in
  let append_node node =
    let (_ : Dom.node Js.t) = a##appendChild node in
    ()
  in
  List.iter append_node items;
  sc a class_;
  si a id;
  Option.iter (fun href -> a##.href := Js.string href) href;
  Option.iter
    (fun on_click ->
      let js_on_click _ =
        on_click ();
        Js._true
      in
      a##.onclick := Dom.handler js_on_click)
    on_click;
  (a :> t)

let span class_ nodes =
  let span = Dom_html.(createSpan document) in
  Option.iter (fun x -> span##.className := Js.string x) class_;
  let append_node node =
    let (_ : Dom.node Js.t) = span##appendChild node in
    ()
  in
  List.iter append_node nodes;

  (span :> t)

let kbd' ?class_ ?id value =
  let kbd = Dom_html.document##createElement (Js.string "kbd") in
  let textv, textup = text' value in
  ignore @@ kbd##appendChild textv;
  sc kbd class_;
  si kbd id;
  ((kbd :> t), textup)

let button ?class_ ?id ?on_click items =
  let button = Dom_html.(createButton document) in
  let append_node node =
    let (_ : Dom.node Js.t) = button##appendChild node in
    ()
  in
  List.iter append_node items;
  sc button class_;
  si button id;
  Option.iter
    (fun on_click ->
      let js_on_click _ =
        on_click ();
        Js._true
      in
      button##.onclick := Dom.handler js_on_click)
    on_click;
  (button :> t)

let td ?class_ ?id ?rowspan items =
  let li = Dom_html.(createTd document) in
  (match rowspan with Some rs -> li##.rowSpan := rs | None -> ());
  List.iter (append_node li) items;
  sc li class_;
  si li id;
  (li :> t)

let tr ?class_ ?id items =
  let li = Dom_html.(createTr document) in
  List.iter (append_node li) items;
  sc li class_;
  si li id;
  (li :> t)

let li ?class_ ?id items =
  let li = Dom_html.(createLi document) in
  List.iter (append_node li) items;
  sc li class_;
  si li id;
  (li :> t)

let ul ?class_ ?id items =
  let ul = Dom_html.(createUl document) in
  (*(match id with None -> () | Some ids -> ul##.id := Js.string ids);*)
  List.iter (append_node ul) items;
  sc ul class_;
  si ul id;
  (ul :> t)

let hi ?class_ ?id i value =
  let hi =
    Dom_html.document##createElement (Js.string ("H" ^ string_of_int i))
  in
  sc hi class_;
  si hi id;
  append_node hi (text value);
  (hi :> t)

let set_items parent (items : t list) =
  (* List.iter
     (fun child ->
       let (_ : Dom.node Js.t) = parent##removeChild child in
       ())
     (Dom.list_of_nodeList parent##.childNodes); *)
  parent##.innerHTML := Js.string "";
  List.iter (append_node parent) items

let p' ?class_ ?id items =
  let p = Dom_html.(createP document) in
  List.iter (append_node p) items;
  sc p class_;
  si p id;
  ((p :> t), set_items p)

let p ?class_ ?id items =
  let p, _ = p' ?class_ ?id items in
  p

let pre_raw ?class_ ?id items =
  let pre = Dom_html.(createPre document) in
  List.iter (append_node pre) items;
  sc pre class_;
  si pre id;
  ( pre,
    fun ?(append = false) elems ->
      if append then List.iter (append_node pre) elems else set_items pre elems
  )

let pre' ?class_ ?id items =
  let d, it = pre_raw ?class_ ?id items in
  ((d :> t), it)

let pre ?class_ ?id items =
  let pre, _ = pre' ?class_ ?id items in
  pre

let div_raw ?class_ ?title ?id items =
  let div = Dom_html.(createDiv document) in
  List.iter (append_node div) items;
  Option.iter (fun title -> div##.title := Js.string title) title;
  sc div class_;
  si div id;
  ( div,
    fun ?(append = false) elems ->
      if append then List.iter (append_node div) elems else set_items div elems
  )

let div' ?class_ ?id ?title items =
  let d, it = div_raw ?class_ ?title ?id items in
  ((d :> t), it)

let div ?class_ ?id ?title items =
  let div, _ = div' ?class_ ?title ?id items in
  div

let table ?class_ ?id items =
  let li = Dom_html.(createTable document) in
  sc li class_;
  si li id;
  List.iter (append_node li) items;
  (li :> t)

let span' ?class_ ?id items =
  let span = Dom_html.(createSpan document) in
  List.iter (append_node span) items;
  sc span class_;
  si span id;
  ((span :> t), set_items span)

let span ?class_ ?id items =
  let span, _ = span' ?class_ ?id items in
  span

let label ?class_ ?id ?forid items =
  let label = Dom_html.(createLabel document) in
  List.iter (append_node label) items;
  Option.iter (fun x -> label##.htmlFor := Js.string x) forid;
  sc label class_;
  si label id;
  (label :> t)

let checkbox_input' ?class_ ?id ?(on_change = fun _ -> ()) checked =
  let input = Dom_html.(createInput ~_type:(Js.string "checkbox") document) in
  input##.checked := Js.bool checked;
  let on_click _ =
    on_change (Js.to_bool input##.checked);
    Js._true
  in
  input##.onclick := Dom.handler on_click;
  sc input class_;
  si input id;
  let set_checked checked = input##.checked := Js.bool checked in
  ((input :> t), set_checked)

let checkbox_input ?class_ ?id ?on_change checked =
  let checkbox_input, _ = checkbox_input' ?class_ ?id ?on_change checked in
  checkbox_input

let radio_input' ?class_ ?id ?(on_change = fun _ -> ()) ?(name = "") checked =
  let input =
    Dom_html.(
      createInput ~name:(Js.string name) ~_type:(Js.string "radio") document)
  in
  input##.checked := Js.bool checked;
  let on_click _ =
    on_change (Js.to_bool input##.checked);
    Js._true
  in
  input##.onclick := Dom.handler on_click;
  sc input class_;
  si input id;
  let set_checked checked = input##.checked := Js.bool checked in
  ((input :> t), set_checked)

let radio_input ?class_ ?id ?on_change ?name checked =
  let radio_input, _ = radio_input' ?class_ ?id ?on_change ?name checked in
  radio_input

let text_input' ?class_ ?id ?(on_change = fun _ -> false) ?(_type = "text")
    value =
  let input = Dom_html.(createInput ~_type:(Js.string _type) document) in
  input##.value := Js.string value;
  (match id with None -> () | Some ids -> input##.id := Js.string ids);
  let on_input _ =
    if on_change (Js.to_string input##.value) then input##.value := Js.string "";
    Js._true
  in
  input##.onchange := Dom.handler on_input;
  sc input class_;
  si input id;
  let set_value value = input##.value := Js.string value in
  ((input :> t), set_value)

let option ?(def = false) value =
  let opt = Dom_html.(createOption document) in
  opt##.value := Js.string value;
  append_node opt (text value);
  if def then opt##.defaultSelected := Js._true;
  (opt :> t)

let select ?class_ ?id ?def ?(on_change = fun _ -> ()) sl =
  let sel = Dom_html.(createSelect document) in
  List.iter
    (fun s ->
      append_node sel
        (option ~def:(match def with None -> false | Some x -> x = s) s))
    sl;
  let on_input _ =
    on_change (Js.to_string sel##.value);
    Js._true
  in
  sel##.oninput := Dom.handler on_input;
  sc sel class_;
  sc sel id;
  (sel :> t)

let text_input ?class_ ?id ?on_change ?_type value =
  let text_input, _ = text_input' ?class_ ?on_change ?_type ?id value in
  text_input

let text_area' ?class_ ?id ?(on_change = fun _ -> ()) ?(is_read_only = false)
    value =
  let input = Dom_html.(createTextarea document) in
  input##.value := Js.string value;
  if is_read_only then
    input##setAttribute (Js.string "readonly") (Js.string "true");
  let on_input _ =
    on_change (Js.to_string input##.value);
    Js._true
  in
  input##.oninput := Dom.handler on_input;
  sc input class_;
  si input id;
  let set_value value = input##.value := Js.string value in
  ((input :> t), set_value)

let text_area ?class_ ?id ?on_change ?is_read_only value =
  let text_area, _ = text_area' ?class_ ?id ?on_change ?is_read_only value in
  text_area

let run html =
  let on_load _ =
    let html = html () in
    let body =
      let find_tag name =
        let elements =
          Dom_html.window##.document##getElementsByTagName (Js.string name)
        in
        let element =
          Js.Opt.get
            (elements##item 0)
            (fun () -> failwith ("find_tag(" ^ name ^ ")"))
        in
        element
      in
      find_tag "body"
    in
    let (_ : t) = body##appendChild html in
    Js._false
  in
  Dom_html.window##.onload := Dom.handler on_load

let blob_of_string st =
  let a = new%js Typed_array.uint8Array (String.length st) in
  for i = 0 to String.length st - 1 do
    Typed_array.set a i (Char.code st.[i])
  done;
  File.blob_from_any [ `arrayBuffer a##.buffer ]

let get_http_request url f =
  let open XmlHttpRequest in
  let req = create () in
  let url = Js.string url in
  req##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        if req##.readyState = DONE then
          if req##.status = 200 then
            (*let msg = File.CoerceTo.string req##.response in
              f (Some msg)*)
            Js.Opt.case
              (File.CoerceTo.arrayBuffer req##.response)
              (fun () -> f (Error "Fail to coerce to arrayBuffer"))
              (fun x -> f (Ok (Typed_array.String.of_arrayBuffer x)))
          else f (Error ("HTTP Error:" ^ string_of_int req##.status)));
  req##_open (Js.string "GET") url Js._true;
  req##.responseType := Js.string "arraybuffer";
  req##send Js.null

let post_http_request url data f =
  let open XmlHttpRequest in
  let req = create () in
  let url = Js.string url in
  req##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        if req##.readyState = DONE then
          if req##.status = 200 then
            Js.Opt.case req##.responseText
              (fun () -> f (Error "Empty answer"))
              (fun x -> f (Ok (Js.to_string x)))
          else f (Error ("HTTP Error:" ^ string_of_int req##.status)));
  req##_open (Js.string "POST") url Js._true;
  req##setRequestHeader (Js.string "Content-Type")
    (Js.string "application/octet-stream");
  req##send_blob (blob_of_string data)

let scroll_to_bottom id =
  let eo = Dom_html.document##getElementById (Js.string id) in
  Js.Opt.iter eo (fun e -> e##.scrollTop := e##.scrollHeight)

let on_key_down f =
  let kp e =
    Js.Optdef.iter e##.code (fun s -> f (Js.to_string s));
    Js._true
  in
  Dom_html.document##.onkeydown := Dom.handler kp

let storage_get s =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> "")
    (fun ls ->
      Js.Opt.case
        (ls##getItem (Js.string s))
        (fun () -> "")
        (fun s -> Js.to_string s))

let storage_set s v =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> false)
    (fun ls ->
      ls##setItem (Js.string s) (Js.string v);
      true)
