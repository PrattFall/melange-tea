external child_nodes : 'a Dom.node_like -> 'b Dom.node_like array
  = "childNodes"
[@@mel.get]

external _first_child : 'a Dom.node_like -> Dom.element Js.nullable
  = "firstChild"
[@@mel.get]

let first_child node = Js.Nullable.toOption (_first_child node)

external _value : 'a Dom.node_like -> string Js.undefined = "nodeValue"
[@@mel.get]

let value node = Js.Undefined.toOption (_value node)

external set_value : 'a Dom.node_like -> string -> unit = "nodeValue"
[@@mel.set]

external append_child : 'a Dom.node_like -> 'b Dom.node_like -> 'b Dom.node_like
  = "appendChild"
[@@mel.send]

external remove_child : 'a Dom.node_like -> 'b Dom.node_like -> 'b Dom.node_like
  = "removeChild"
[@@mel.send]

external insert_before :
  'a Dom.node_like -> 'b Dom.node_like -> 'c Dom.node_like -> 'b Dom.node_like
  = "insertBefore"
[@@mel.send]

external _set_attribute : 'a Dom.element_like -> string -> string -> unit
  = "setAttribute"
[@@mel.send]

external _set_attribute_ns :
  'a Dom.element_like -> string -> string -> string -> unit = "setAttributeNS"
[@@mel.send]

let set_attribute ?(namespace = "") elem key value =
  match namespace with
  | "" -> _set_attribute elem key value
  | ns -> _set_attribute_ns elem ns key value

external _remove_attribute : 'a Dom.element_like -> string -> unit
  = "removeAttribute"
[@@mel.send]

external _remove_attribute_ns : 'a Dom.element_like -> string -> string -> unit
  = "removeAttributeNS"
[@@mel.send]

let remove_attribute ?(namespace = "") elem key =
  match namespace with
  | "" -> _remove_attribute elem key
  | ns -> _remove_attribute_ns elem ns key

external add_event_listener :
  'a Dom.eventTarget_like -> string -> ('e Dom.event_like -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external remove_event_listener :
  'a Dom.eventTarget_like -> string -> ('e Dom.event_like -> unit) -> unit
  = "removeEventListener"
[@@mel.send]

external prop : 'a Dom.element_like -> 'k -> 'v = "" [@@mel.get_index]

external set_prop : 'a Dom.element_like -> 'k -> 'v -> unit = ""
[@@mel.set_index]

external style : Dom.element -> Dom.cssStyleDeclaration = "style" [@@mel.get]

external get_style : Dom.cssStyleDeclaration -> string -> string Js.null = ""
[@@mel.get_index]

external set_style : Dom.cssStyleDeclaration -> string -> string Js.null -> unit
  = ""
[@@mel.set_index]

external _set_property :
  Dom.cssStyleDeclaration -> string -> string Js.null -> string -> unit
  = "setProperty"
[@@mel.send]

let set_style_property ?(priority = "") elem prop_name value =
  _set_property (style elem) prop_name (Js.Null.fromOption value) priority

external focus : 'a Dom.element_like -> unit = "focus" [@@mel.send]

external _checked : 'a Dom.element_like -> bool Js.undefined = "checked"
[@@mel.get]

let checked node = Js.Undefined.toOption (_checked node)

external dataset : 'a Dom.element_like -> string Js.Dict.t = "dataset" [@@mel.get]

(* The `delete` keyword is not supported by Js.Dict *)
let remove_from_dataset = [%mel.raw {|
    function (elem, key) {
        delete elem.dataset[key];
    }
|}]

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
