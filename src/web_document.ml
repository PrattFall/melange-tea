(* TODO:  Polyfill document if it is missing, like on node or in native *)

external document : Dom.document = "document"
external body : Dom.document -> 'a Dom.node_like = "body" [@@mel.get]

external _create_element : Dom.document -> string -> 'a Dom.element_like
  = "createElement"
[@@mel.send]

external _create_element_ns :
  Dom.document -> string -> string -> 'a Dom.element_like = "createElementNS"
[@@mel.send]

external _create_comment : Dom.document -> string -> 'a Dom.node_like
  = "createComment"
[@@mel.send]

external _create_text_node : Dom.document -> string -> 'a Dom.node_like
  = "createTextNode"
[@@mel.send]

external _get_element_by_id :
  Dom.document -> string -> 'a Dom.element_like Js.nullable = "getElementById"
[@@mel.send]

external _location : Dom.document -> Web_location.t = "location" [@@mel.get]

let node = (document :> 'a Dom._document Dom.node_like)
let create_comment value = _create_comment document value
let create_text_node value = _create_text_node document value

let get_element_by_id id =
  _get_element_by_id document id |> Js.Nullable.toOption

let create_element ?(namespace = "") typ =
  match namespace with
  | "" -> _create_element document typ
  | ns -> _create_element_ns document ns typ

let get_location () = _location document
