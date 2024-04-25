(* TODO:  Polyfill document if it is missing, like on node or in native *)

external document : Dom.document = "document"

external body : Dom.document -> 'a Dom.node_like = "body" [@@mel.get]

let node = (document :> 'a Dom._document Dom.node_like)

external create_element' : Dom.document -> string -> 'a Dom.element_like
  = "createElement"
[@@mel.send]

external create_element_ns' :
  Dom.document -> string -> string -> 'a Dom.element_like = "createElementNS"
[@@mel.send]

let create_element ?(namespace = "") typ =
  match namespace with
  | "" -> create_element' document typ
  | ns -> create_element_ns' document ns typ

external create_comment' : Dom.document -> string -> 'a Dom.node_like
  = "createComment"
[@@mel.send]

let create_comment value = create_comment' document value

external create_text_node' : Dom.document -> string -> 'a Dom.node_like
  = "createTextNode"
[@@mel.send]

let create_text_node value = create_text_node' document value

external get_element_by_id' : Dom.document -> string -> 'a Dom.element_like Js.nullable
  = "getElementById"
[@@mel.send]

let get_element_by_id id =
  get_element_by_id' document id |> Js.Nullable.toOption

external location : Dom.document -> Web_location.t = "location" [@@mel.get]

let get_location () = location document
