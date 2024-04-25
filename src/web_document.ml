(* TODO:  Polyfill document if it is missing, like on node or in native *)

external document : Dom.document = "document"
external body : Dom.document -> Dom.node = "body" [@@mel.get]

external create_element : Dom.document -> string -> Dom.element
  = "createElement"
[@@mel.send]

external create_element_ns : Dom.document -> string -> string -> Dom.element
  = "createElementNS"
[@@mel.send]

let create_element' ?(namespace = "") doc typ =
  match namespace with
  | "" -> create_element doc typ
  | ns -> create_element_ns doc ns typ

external create_comment : Dom.document -> string -> Dom.element
  = "createComment"
[@@mel.send]

external create_text_node : Dom.document -> string -> Dom.element
  = "createTextNode"
[@@mel.send]

external get_element_by_id : Dom.document -> string -> Dom.element Js.nullable
  = "getElementById"
[@@mel.send]

let get_element_by_id' id =
  get_element_by_id document id |> Js.Nullable.toOption

external location : Dom.document -> Web_location.t = "location" [@@mel.get]

let get_location () =
    location document
