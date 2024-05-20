open Vdom
module Cmds = Tea_html_cmds
module Attributes = Tea_svg_attributes

let svgNamespace = "http://www.w3.org/2000/svg"

(* Nodes *)

let noNode = Node.empty
let text str = Node.text str
let lazy1 key gen = Node.lazyGen key gen

let node ?(key = "") ?(unique = "") props nodes =
  Node.node ~namespace:svgNamespace ~key ~unique props nodes

let svg ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "svg" props nodes

(* Animation elements *)

let foreignObject ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "foreignObject" props nodes

let animate ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "animate" props nodes

let animateColor ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "animateColor" props nodes

let animateMotion ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "animateMotion" props nodes

let animateTransform ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "animateTransform" props nodes

let mpath ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "mpath" props nodes

let set ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "set" props nodes

(* Container elements *)

let a ?(key = "") ?(unique = "") props nodes = node ~key ~unique "a" props nodes

let defs ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "defs" props nodes

let g ?(key = "") ?(unique = "") props nodes = node ~key ~unique "g" props nodes

let marker ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "marker" props nodes

let mask ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "mask" props nodes

let missingGlyph ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "missingGlyph" props nodes

let pattern ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "pattern" props nodes

let switch ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "switch" props nodes

let symbol ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "symbol" props nodes

(* Descriptive elements *)

let desc ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "desc" props nodes

let metadata ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "metadata" props nodes

let title ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "title" props nodes

(* Filter primitive elements *)

let feBlend ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feBlend" props nodes

let feColorMatrix ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feColorMatrix" props nodes

let feComponentTransfer ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feComponentTransfer" props nodes

let feComposite ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feComposite" props nodes

let feConvolveMatrix ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feConvolveMatrix" props nodes

let feDiffuseLighting ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feDiffuseLighting" props nodes

let feDisplacementMap ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feDisplacementMap" props nodes

let feFlood ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feFlood" props nodes

let feFuncA ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feFuncA" props nodes

let feFuncB ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feFuncB" props nodes

let feFuncG ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feFuncG" props nodes

let feFuncR ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feFuncR" props nodes

let feGaussianBlur ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feGaussianBlur" props nodes

let feImage ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feImage" props nodes

let feMerge ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feMerge" props nodes

let feMergeNode ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feMergeNode" props nodes

let feMorphology ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feMorphology" props nodes

let feOffset ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feOffset" props nodes

let feSpecularLighting ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feSpecularLighting" props nodes

let feTile ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feTile" props nodes

let feTurbulence ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feTurbulence" props nodes

(* Font elements *)

let font ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "font" props nodes

let fontFace ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fontFace" props nodes

let fontFaceFormat ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fontFaceFormat" props nodes

let fontFaceName ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fontFaceName" props nodes

let fontFaceSrc ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fontFaceSrc" props nodes

let fontFaceUri ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fontFaceUri" props nodes

let hkern ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "hkern" props nodes

let vkern ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "vkern" props nodes

(* Gradient elements *)

let linearGradient ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "linearGradient" props nodes

let radialGradient ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "radialGradient" props nodes

let stop ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "stop" props nodes

(* Graphics elements *)

let circle ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "circle" props nodes

let ellipse ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "ellipse" props nodes

let svgimage ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "image" props nodes

let line ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "line" props nodes

let path ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "path" props nodes

let polygon ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "polygon" props nodes

let polyline ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "polyline" props nodes

let rect ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "rect" props nodes

let use ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "use" props nodes

(* Light source elements *)

let feDistantLight ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feDistantLight" props nodes

let fePointLight ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "fePointLight" props nodes

let feSpotLight ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "feSpotLight" props nodes

(* Text content elements *)

let altGlyph ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "altGlyph" props nodes

let altGlyphDef ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "altGlyphDef" props nodes

let altGlyphItem ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "altGlyphItem" props nodes

let glyph ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "glyph" props nodes

let glyphRef ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "glyphRef" props nodes

let textPath ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "textPath" props nodes

let text' ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "text" props nodes

let tref ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tref" props nodes

let tspan ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "tspan" props nodes

(* Uncategorized elements *)

let clipPath ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "clipPath" props nodes

let svgcolorProfile ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "colorProfile" props nodes

let cursor ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "cursor" props nodes

let filter ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "filter" props nodes

let script ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "script" props nodes

let style ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "style" props nodes

let view ?(key = "") ?(unique = "") props nodes =
  node ~key ~unique "view" props nodes
