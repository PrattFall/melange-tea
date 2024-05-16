type position = { x : int; y : int }

let position (ev : 'e Dom.event_like) : position =
  { x = Web_event.page_x ev; y = Web_event.page_y ev }

let registerGlobal name key tagger =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in

    let fn (ev : 'e Dom.event_like) : 'msg option =
      position ev |> Option.some |> Option.map tagger
    in

    let handler = EventHandler.callback key fn in

    let elem = Web.Document.node in

    let cache = EventHandler.register callbacks elem name handler in

    fun () -> EventHandler.unregister elem name cache |> ignore
  in

  Tea_sub.registration key enableCall

let clicks ?(key = "") tagger = registerGlobal "click" key tagger
let moves ?(key = "") tagger = registerGlobal "mousemove" key tagger
let downs ?(key = "") tagger = registerGlobal "mousedown" key tagger
let ups ?(key = "") tagger = registerGlobal "mouseup" key tagger
