(** This file is organized roughly in order of popularity. The tags which you'd
expect to use frequently will be closer to the top. *)

open Vdom.Property
module Cmds = Tea_html_cmds
module Node = Web_node

let map = Tea_app.map

(** {1 Primitives} *)

let text str = Vdom.Node.text str

let node ?(key = "") ?(unique = "") tagName props nodes =
  Vdom.Node.fullnode "" tagName key unique props nodes

let noNode = Vdom.Node.empty
let lazy1 key gen = Vdom.Node.lazyGen key gen

(** {1 Tags} *)

(** {2 Headers} *)

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

(** {2 Grouping Content} *)

let div ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "div" props nodes

let p ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "p" props nodes

let hr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "hr" props nodes

let pre ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "pre" props nodes

let blockquote ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "blockquote" props nodes

(** {2 Text} *)

let span ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "span" props nodes

let a ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "a" props nodes

let code ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "code" props nodes

let em ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "em" props nodes

let strong ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "strong" props nodes

let i ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "i" props nodes

let b ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "b" props nodes

let u ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "u" props nodes

let sub ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "sub" props nodes

let sup ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "sup" props nodes

let br props = node ~key:"br" ~unique:"br" "br" props []

let br' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "br" props nodes

(** {2 Lists} *)

let ol ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ol" props nodes

let ul ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ul" props nodes

let li ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "li" props nodes

let dl ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dl" props nodes

let dt ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dt" props nodes

let dd ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dd" props nodes

(** {2 Embedded Content} *)

let img ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "img" props nodes

let iframe ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "iframe" props nodes

let canvas ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "canvas" props nodes

let math ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "math" props nodes

(** {2 Form and inputs} *)

let form ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "form" props nodes

let input' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "input" props nodes

let textarea ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "textarea" props nodes

let button ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "button" props nodes

let select ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "select" props nodes

let option' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "option" props nodes

let optgroup ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "optgroup" props nodes

let label ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "label" props nodes

let fieldset ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fieldset" props nodes

let legend ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "legend" props nodes

(** {2 Sections} *)

let section ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "section" props nodes

let nav ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "nav" props nodes

let article ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "article" props nodes

let aside ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "aside" props nodes

let header ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "header" props nodes

let footer ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "footer" props nodes

let address ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "address" props nodes

let main ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "main" props nodes

let body ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "body" props nodes

(** {2 Figures} *)

let figure ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "figure" props nodes

let figcaption ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "figcaption" props nodes

(** {2 Tables} *)

let table ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "table" props nodes

let caption ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "caption" props nodes

let colgroup ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "colgroup" props nodes

let col ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "col" props nodes

let tbody ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tbody" props nodes

let thead ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "thead" props nodes

let tfoot ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tfoot" props nodes

let tr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tr" props nodes

let th ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "th" props nodes

let td ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "td" props nodes

(** {2 Less common inputs} *)

let datalist ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "datalist" props nodes

let keygen ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "keygen" props nodes

let output ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "output" props nodes

let progress ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "progress" props nodes

let meter ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "meter" props nodes

(** {2 Audio and Video} *)

let audio ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "audio" props nodes

let video ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "video" props nodes

let source ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "source" props nodes

let track ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "track" props nodes

(** {2 Embedded objects} *)

let embed ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "embed" props nodes

let object' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "object" props nodes

let param ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "param" props nodes

(** {2 Text edits} *)

let ins ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ins" props nodes

let del ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "del" props nodes

(** {2 Semantic text} *)

let small ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "small" props nodes

let cite ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "cite" props nodes

let dfn ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "dfn" props nodes

let abbr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "abbr" props nodes

let time ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "time" props nodes

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

(** {2 Less common text tags} *)

let mark ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "mark" props nodes

let ruby ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ruby" props nodes

let rt ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "rt" props nodes

let rp ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "rp" props nodes

let bdi ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "bdi" props nodes

let bdo ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "bdo" props nodes

let wbr ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "wbr" props nodes

(** {2 Interactive elements} *)

let details ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "details" props nodes

let summary ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "summary" props nodes

let menuitem ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "menuitem" props nodes

let menu ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "menu" props nodes

(** {2 Header elements} *)

let meta ?(key = "") ?(unique = "") props =
  node ~key ~unique "meta" props []

let style ?(key = "") ?(unique = "") props content =
  node ~key ~unique "style" props [ text content ]

let title ?(key = "") ?(unique = "") props content =
  node ~key ~unique "title" props [ text content ]

let link ?(key = "") ?(unique = "") props =
  node ~key ~unique "link" props []

(** Helper functions for HTML attributes. They are organized roughly by category. *)
module Attributes = struct
  (** {1 Primitives} *)

  let noProp = Vdom.Property.empty
  let style = Vdom.Property.style
  let styles = Vdom.Property.styles

  (** {1 Super common attributes} *)

  let class' name = prop "className" name

  let classList classes =
    classes
    |> List.filter (fun (_fst, snd) -> snd)
    |> List.map (fun (fst, _snd) -> fst)
    |> String.concat " " |> class'

  let id str = prop "id" str
  let title str = attribute "" "title" str
  let hidden b = if b then prop "hidden" "hidden" else noProp

  (** {1 Inputs} *)

  let type' typ = prop "type" typ
  let value str = prop "value" str
  let defaultValue str = prop "defaultValue" str
  let checked b = if b then prop "checked" "checked" else noProp
  let placeholder str = prop "placeholder" str
  let selected b = if b then attribute "" "selected" "true" else noProp

  (** {1 Input helpers} *)

  let accept c = attribute "" "accept" c
  let acceptCharset c = attribute "" "accept-charset" c
  let action a = prop "action" a
  let autocomplete b = prop "autocomplete" (if b then "on" else "off")
  let autofocus b = if b then prop "autofocus" "autofocus" else noProp
  let disabled b = if b then attribute "" "disabled" "true" else noProp
  let enctype encoding = attribute "" "enctype" encoding
  let formaction url = attribute "" "formaction" url
  let list value = attribute "" "list" value
  let minlength n = attribute "" "minlength" (string_of_int n)
  let maxlength n = attribute "" "maxlength" (string_of_int n)
  let method' m = prop "method" m
  let multiple b = if b then prop "multiple" "multiple" else noProp
  let name str = prop "name" str
  let novalidate b = if b then prop "novalidate" "novalidate" else noProp
  let pattern p = prop "pattern" p
  let readonly b = if b then attribute "" "readonly" "readonly" else noProp
  let required b = if b then attribute "" "required" "required" else noProp
  let size n = attribute "" "size" (string_of_int n)
  let for' str = prop "htmlFor" str
  let form value = attribute "" "form" value

  (** {1 Input ranges} *)

  let max value = attribute "" "max" value
  let min value = attribute "" "min" value
  let step value = attribute "" "step" value

  (** {1 Textarea} *)

  let cols n = attribute "" "cols" (string_of_int n)
  let rows n = attribute "" "rows" (string_of_int n)
  let wrap value = prop "wrap" value

  (** {1 Links and areas} *)

  (* `href` is actually an attribute, not a property, but need it here for Elm compat... *)
  let href str = attribute "" "href" str
  let target t = prop "target" t
  let download b = if b then prop "download" "" else noProp
  let downloadAs name = prop "download" name
  let hreflang code = prop "hreflang" code
  let media value = attribute "" "media" value
  let ping url = prop "ping" url
  let rel value = attribute "" "rel" value

  (** {1 Maps} *)

  let ismap b = if b then prop "ismap" "ismap" else noProp
  let usemap name = prop "usemap" name
  let shape value = prop "shape" value
  let coords value = prop "coords" value

  (** {1 Embedded content} *)

  (* `src` is actually an attribute, not a property, but need it here for Elm compat... *)
  let src str = attribute "" "src" str
  let height n = attribute "" "height" (string_of_int n)
  let width n = attribute "" "width" (string_of_int n)
  let alt value = prop "alt" value

  (** {1 Audio and Video} *)

  let autoplay b = if b then prop "autoplay" "autoplay" else noProp
  let controls b = if b then prop "controls" "controls" else noProp
  let loop b = if b then prop "loop" "loop" else noProp
  let preload value = prop "preload" value
  let poster url = prop "poster" url
  let default b = if b then prop "default" "default" else noProp
  let kind value = prop "kind" value
  let srclang code = prop "srclang" code

  (** {1 IFrames} *)

  let sandbox value = prop "sandbox" value
  let seamless b = if b then prop "seamless" "seamless" else noProp
  let srcdoc value = prop "srcdoc" value

  (** {1 Ordered lists} *)

  let reversed b = if b then prop "reversed" "reversed" else noProp
  let start n = prop "start" (string_of_int n)

  (** {1 Tables} *)

  let colspan n = attribute "" "colspan" (string_of_int n)
  let rowspan n = attribute "" "rowspan" (string_of_int n)
  let headers value = prop "headers" value
  let scope value = prop "scope" value
  let align value = prop "align" value

  (** {1 Header stuff} *)

  let async b = if b then prop "async" "async" else noProp
  let charset value = attribute "" "charset" value
  let content value = attribute "" "content" value
  let defer b = if b then prop "defer" "defer" else noProp
  let httpEquiv value = prop "http-equiv" value
  let language value = prop "language" value
  let scoped value = prop "scoped" value

  (** {1 Less common global attributes} *)

  let accesskey ch = prop "accesskey" (String.make 1 ch)

  let contenteditable b =
    if b then prop "contenteditable" "contenteditable" else noProp

  let contextmenu id = attribute "" "contextmenu" id
  let dir value = prop "dir" value
  let draggable value = attribute "" "draggable" value
  let dropzone value = prop "dropzone" value
  let itemprop value = attribute "" "itemprop" value
  let lang code = prop "lang" code
  let spellcheck b = if b then prop "spellcheck" "spellcheck" else noProp
  let tabindex n = attribute "" "tabindex" (string_of_int n)

  (** {1 Key generation} *)

  let challenge value = attribute "" "challenge" value
  let keytype value = prop "keytype" value

  (** {1 Miscellaneous} *)

  let cite url = prop "cite" url
  let datetime value = attribute "" "datetime" value
  let pubdate value = attribute "" "pubdate" value
  let manifest value = attribute "" "manifest" value
end

module Events = struct
  (** {1 Primitives} *)

  let onCB eventName key cb = onCB eventName key cb
  let onMsg eventName msg = onMsg eventName msg
  let on = Tea_html.on
  let onWithOptions = Tea_html.onWithOptions
  let defaultOptions = Tea_html.defaultOptions
  let targetValue = Tea_html.targetValue
  let targetChecked = Tea_html.targetChecked
  let keyCode = Tea_html.keyCode

  let preventDefaultOn ?(key = "") eventName decoder =
    onWithOptions ~key eventName
      { defaultOptions with preventDefault = true }
      decoder

  (** {1 Mouse helpers} *)

  let onClick msg = onMsg "click" msg
  let onDoubleClick msg = onMsg "dblclick" msg
  let onMouseDown msg = onMsg "mousedown" msg
  let onMouseUp msg = onMsg "mouseup" msg
  let onMouseEnter msg = onMsg "mouseenter" msg
  let onMouseLeave msg = onMsg "mouseleave" msg
  let onMouseOver msg = onMsg "mouseover" msg
  let onMouseOut msg = onMsg "mouseout" msg

  (** {1 Form helpers} *)

  let onInputOpt ?(key = "") msg =
    onCB "input" key (fun ev ->
        match Web_event.target ev with
        | None -> None
        | Some target -> (
            match Node.value target with
            | None -> None
            | Some value -> msg value))

  let onInput ?(key = "") msg = onInputOpt ~key (fun ev -> Some (msg ev))

  let onCheckOpt ?(key = "") msg =
    onCB "change" key (fun ev ->
        match Web_event.target ev with
        | None -> None
        | Some target -> (
            match Node.checked target with
            | None -> None
            | Some value -> msg value))

  let onCheck ?(key = "") msg = onCheckOpt ~key (fun ev -> Some (msg ev))

  let onChangeOpt ?(key = "") msg =
    onCB "change" key (fun ev ->
        match Web_event.target ev with
        | None -> None
        | Some target -> (
            match Node.value target with
            | None -> None
            | Some value -> msg value))

  let onChange ?(key = "") msg = onChangeOpt ~key (fun ev -> Some (msg ev))
  let onSubmit msg = preventDefaultOn "submit" (Tea_json.Decoder.succeed msg)

  (** {1 Focus helpers} *)

  let onBlur msg = onMsg "blur" msg
  let onFocus msg = onMsg "focus" msg
end