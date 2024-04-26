open Vdom
module Node = Web_node
module Cmds = Tea_html_cmds

let map = Tea_app.map

(* Nodes *)

let noProp = Property.empty
let noNode = Vdom.Node.empty
let text str = Vdom.Node.text str
let lazy1 key gen = Vdom.Node.lazyGen key gen

let node ?(key = "") ?(unique = "") tagName  props nodes =
  Vdom.Node.fullnode "" tagName key unique props nodes

(* let embedProgram main = custom *)

(* HTML Elements *)

let br props = node ~key:"br" ~unique:"br" "br" props []

let div ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "div" props nodes

let span ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "span" props nodes

let p ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "p" props nodes

let pre ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "pre" props nodes

let a ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "a" props nodes

let section ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "section" props nodes

let header ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "header" props nodes

let footer ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "footer" props nodes

let h1 ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "h1" props nodes

let h2 ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "h2" props nodes

let h3 ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "h3" props nodes

let h4 ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "h4" props nodes

let h5 ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "h5" props nodes

let h6 ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "h6" props nodes

let i ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "i" props nodes

let strong ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "strong" props nodes

let button ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "button" props nodes

let input' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "input" props nodes

let textarea ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "textarea" props nodes

let label ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "label" props nodes

let ul ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ul" props nodes

let ol ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ol" props nodes

let li ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "li" props nodes

let table ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "table" props nodes

let thead ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "thead" props nodes

let tfoot ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tfoot" props nodes

let tbody ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tbody" props nodes

let th ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "th" props nodes

let tr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tr" props nodes

let td ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "td" props nodes

let progress ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "progress" props nodes

let img ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "img" props nodes

let select ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "select" props nodes

let option' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "option" props nodes

let form ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "form" props nodes

let nav ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "nav" props nodes

let main ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "main" props nodes

let aside ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "aside" props nodes

let article ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "article" props nodes

let details ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "details" props nodes

let figcaption ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "figcaption" props nodes

let figure ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "figure" props nodes

let mark ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "mark" props nodes

let summary ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "summary" props nodes

let time ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "time" props nodes

let hr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "hr" props nodes

let blockquote ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "blockquote" props nodes

let code ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "code" props nodes

let em ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "em" props nodes

let b ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "b" props nodes

let u ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "u" props nodes

let sub ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "sub" props nodes

let sup ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "sup" props nodes

let dl ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dl" props nodes

let dt ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dt" props nodes

let dd ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dd" props nodes

let iframe ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "iframe" props nodes

let canvas ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "canvas" props nodes

let address ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "address" props nodes

let caption ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "caption" props nodes

let colgroup ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "colgroup" props nodes

let col ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "col" props nodes

let fieldset ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fieldset" props nodes

let legend ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "legend" props nodes

let datalist ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "datalist" props nodes

let optgroup ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "optgroup" props nodes

let output ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "output" props nodes

let meter ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "meter" props nodes

let audio ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "audio" props nodes

let video ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "video" props nodes

let source ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "source" props nodes

let track ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "track" props nodes

let embed ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "embed" props nodes

let object' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "object" props nodes

let param ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "param" props nodes

let ins ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ins" props nodes

let del ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "del" props nodes

let small ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "small" props nodes

let cite ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "cite" props nodes

let dfn ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dfn" props nodes

let abbr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "abbr" props nodes

let var' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "var" props nodes

let samp ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "samp" props nodes

let kbd ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "kbd" props nodes

let s ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "s" props nodes

let q ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "q" props nodes

let rt ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "rt" props nodes

let bdi ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "bdi" props nodes

let bdo ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "bdo" props nodes

let wbr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "wbr" props nodes

let menuitem ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "menuitem" props nodes

let menu ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "menu" props nodes

(* Properties *)

let id str = Property.prop "id" str

(* `href` is actually an attribute, not a property, but need it here for Elm compat... *)
let href str = Property.attribute "" "href" str

(* `src` is actually an attribute, not a property, but need it here for Elm compat... *)
let src str = Property.attribute "" "src" str
let title str = Property.attribute "" "title" str
let class' name = Property.prop "className" name

let classList classes =
  classes
  |> List.filter (fun (_fst, snd) -> snd)
  |> List.map (fun (fst, _snd) -> fst)
  |> String.concat " " |> class'

let type' typ = Property.prop "type" typ
let style key value = Property.style key value
let styles s = Property.styles s
let placeholder str = Property.prop "placeholder" str
let autofocus b = if b then Property.prop "autofocus" "autofocus" else noProp
let value str = Property.prop "value" str
let name str = Property.prop "name" str
let checked b = if b then Property.prop "checked" "checked" else noProp
let for' str = Property.prop "htmlFor" str
let hidden b = if b then Property.prop "hidden" "hidden" else noProp
let target t = Property.prop "target" t
let action a = Property.prop "action" a
let method' m = Property.prop "method" m

(* Events *)

let onCB eventName key cb = Property.onCB eventName key cb
let onMsg eventName msg = Property.onMsg eventName msg

let onInputOpt ?(key = "") msg =
  onCB "input" key (fun ev ->
      Web_event.target ev |> Option.map Node.value |> Option.map msg)

let onInput ?(key = "") msg = onInputOpt ~key (fun ev -> Some (msg ev))

let onChangeOpt ?(key = "") msg =
  onCB "change" key (fun ev ->
      Web_event.target ev |> Option.map Node.value |> Option.map msg)

let onChange ?(key = "") msg = onChangeOpt ~key (fun ev -> Some (msg ev))
let onClick msg = onMsg "click" msg
let onDoubleClick msg = onMsg "dblclick" msg
let onBlur msg = onMsg "blur" msg
let onFocus msg = onMsg "focus" msg

let onCheckOpt ?(key = "") msg =
  onCB "change" key (fun ev ->
      Web_event.target ev |> Option.map Node.checked |> Option.map msg)

let onCheck ?(key = "") msg = onCheckOpt ~key (fun ev -> Some (msg ev))
let onMouseDown msg = onMsg "mousedown" msg
let onMouseUp msg = onMsg "mouseup" msg
let onMouseEnter msg = onMsg "mouseenter" msg
let onMouseLeave msg = onMsg "mouseleave" msg
let onMouseOver msg = onMsg "mouseover" msg
let onMouseOut msg = onMsg "mouseout" msg

type options = { stopPropagation : bool; preventDefault : bool }

let defaultOptions = { stopPropagation = false; preventDefault = false }

let onWithOptions ~(key : string) eventName (options : options) decoder =
  onCB eventName key (fun event ->
      if options.stopPropagation then Web_event.stopPropagation event;
      if options.preventDefault then Web_event.preventDefault event;
      event |> Tea_json.Decoder.decodeEvent decoder |> Result.to_option)

let on ~(key : string) eventName decoder =
  onWithOptions ~key eventName defaultOptions decoder

let targetValue =
  Tea_json.Decoder.at [ "target"; "value" ] Tea_json.Decoder.string

let targetChecked =
  Tea_json.Decoder.at [ "target"; "checked" ] Tea_json.Decoder.bool

let keyCode = Tea_json.Decoder.field "keyCode" Tea_json.Decoder.int

module Attributes = struct
  let max value = Property.attribute "" "max" value
  let min value = Property.attribute "" "min" value
  let step value = Property.attribute "" "step" value
  let disabled b = if b then Property.attribute "" "disabled" "true" else noProp
  let selected b = if b then Property.attribute "" "selected" "true" else noProp
  let acceptCharset c = Property.attribute "" "accept-charset" c
  let rel value = Property.attribute "" "rel" value
end
