type position = { x : int; y : int }

let position =
  Tea_json.Decoder.(
    map2 (fun x y -> { x; y }) (field "pageX" int) (field "pageY" int))

let registerGlobal name key tagger =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in

    let fn ev =
      Tea_json.Decoder.decodeEvent position ev
      |> Result.to_option |> Option.map tagger
    in

    let handler = EventHandler.EventHandlerCallback (key, fn) in

    let elem = Web.Document.node in

    let cache = EventHandler.register callbacks elem name handler in

    fun () -> EventHandler.unregister elem name cache |> ignore
  in

  Tea_sub.registration key enableCall

let clicks ?(key = "") tagger = registerGlobal "click" key tagger
let moves ?(key = "") tagger = registerGlobal "mousemove" key tagger
let downs ?(key = "") tagger = registerGlobal "mousedown" key tagger
let ups ?(key = "") tagger = registerGlobal "mouseup" key tagger
