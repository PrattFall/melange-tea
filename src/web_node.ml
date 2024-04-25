external children : Dom.element -> Dom.element Js.Array.t = "children"
[@@mel.get]

external child_nodes : 'a Dom.node_like -> 'b Dom.node_like Js.Array.t
  = "childNodes"
[@@mel.get]

external child_elements : Dom.node -> Dom.element Js.Array.t = "childNodes"
[@@mel.get]

external has_children : 'a Dom.node_like -> bool = "hasChildNodes" [@@mel.send]

external first_child : 'a Dom.node_like -> Dom.element Js.nullable
  = "firstChild"
[@@mel.get]

external last_child : 'a Dom.node_like -> 'b Dom.node_like Js.nullable
  = "lastChild"
[@@mel.get]

external next_sibling : 'a Dom.node_like -> 'b Dom.node_like Js.nullable
  = "nextSibling"
[@@mel.get]

external previous_sibling : 'a Dom.node_like -> 'b Dom.node_like Js.nullable
  = "previousSibling"
[@@mel.get]

external parent_node : 'a Dom.node_like -> 'b Dom.node_like Js.nullable
  = "parentNode"
[@@mel.get]

external parent_element : Dom.element -> Dom.element Js.nullable
  = "parentElement"
[@@mel.get]

external name : 'a Dom.node_like -> string Js.undefined = "nodeName" [@@mel.get]
external value : 'a Dom.node_like -> string Js.undefined = "value" [@@mel.get]
external set_value : 'a Dom.node_like -> string -> unit = "value" [@@mel.set]

external type' : 'a Dom.node_like -> string Js.undefined = "nodeType"
[@@mel.get]

external text : 'a Dom.node_like -> string Js.undefined = "textContent"
[@@mel.get]

external append_child : 'a Dom.node_like -> 'b Dom.node_like -> 'b Dom.node_like
  = "appendChild"
[@@mel.send]

external remove_child : 'a Dom.node_like -> 'b Dom.node_like -> 'b Dom.node_like
  = "removeChild"
[@@mel.send]

external remove : 'a Dom.node_like -> unit = "remove" [@@mel.send]
external clone : 'a Dom.node_like -> 'a Dom.node_like = "cloneNode" [@@mel.send]

external contains : 'a Dom.node_like -> 'b Dom.node_like -> bool = "contains"
[@@mel.send]

external insert_before :
  'a Dom.node_like -> 'b Dom.node_like -> 'c Dom.node_like -> 'b Dom.node_like
  = "insertBefore"
[@@mel.send]

external set_attribute : 'a Dom.element_like -> string -> string -> unit
  = "setAttribute"
[@@mel.send]

external set_attribute_ns :
  'a Dom.element_like -> string -> string -> string -> unit = "setAttributeNS"
[@@mel.send]

let set_attribute' ?(namespace = "") elem key value =
  match namespace with
  | "" -> set_attribute elem key value
  | ns -> set_attribute_ns elem ns key value

external remove_attribute : 'a Dom.element_like -> string -> unit
  = "removeAttribute"
[@@mel.send]

external remove_attribute_ns : 'a Dom.element_like -> string -> string -> unit
  = "removeAttributeNS"
[@@mel.send]

let remove_attribute' ?(namespace = "") elem key =
  match namespace with
  | "" -> remove_attribute elem key
  | ns -> remove_attribute_ns elem ns key

type style =
  < setProperty : Web_json.t Js.undefined
        [@mel.get] (* TODO:  Revamp this and the next line... *)
  ; setProperty__ : string -> string Js.null -> string Js.null -> unit
        [@mel.meth] >
  Js.t

external get_style : style -> string -> string Js.null = "" [@@mel.get_index]

external set_style : style -> string -> string Js.null -> unit = ""
[@@mel.set_index]

external style : Dom.element -> style = "style" [@@mel.get]

type dom_event = Dom.element Web_event.t
type dom_event_cb = Dom.element Web_event.cb

external add_event_listener :
  'a Dom.eventTarget_like -> string -> dom_event_cb -> unit = "addEventListener"
[@@mel.send]

external remove_event_listener :
  'a Dom.eventTarget_like -> string -> dom_event_cb -> unit
  = "removeEventListener"
[@@mel.send]

external prop : 'a Dom.element_like -> 'k -> 'v = "" [@@mel.get_index]

external set_prop : 'a Dom.element_like -> 'k -> 'v -> unit = ""
[@@mel.set_index]

let set_style_property ?(priority = false) elem key value =
  let elem_style = style elem in

  match Js.Undefined.toOption elem_style##setProperty with
  | None ->
      set_style elem_style key value
      (* TODO:  Change this to setAttribute sometime, maybe... *)
  | Some _valid ->
      elem_style##setProperty__ key value
        (if priority then Js.Null.return "important" else Js.Null.empty)

external focus : 'a Dom.element_like -> unit = "focus" [@@mel.send]

external checked : 'a Dom.element_like -> bool Js.undefined = "checked"
[@@mel.get]

let remove_polyfill : unit -> unit =
 fun () ->
  [%mel.raw
    {|
  // remove polyfill
  (function() {
    if (!('remove' in Element.prototype)) {
      Element.prototype.remove = function() {

        if (this.parentNode) {
          this.parentNode.removeChild(this);

        }
      };
    };
  }())
  |}]
