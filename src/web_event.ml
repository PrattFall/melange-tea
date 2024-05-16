external keyCode : 'e Dom.event_like -> int = "keyCode" [@@mel.get]

external preventDefault : 'e Dom.event_like -> unit = "preventDefault"
[@@mel.send]

external stopPropagation : 'e Dom.event_like -> unit = "stopPropagation"
[@@mel.send]

type 'e callback = 'e Dom.event_like -> unit

type options = bool

let emptyCallback : 'e callback = fun _ev -> ()

type popstateEvent = < > Js.t
type popstateCb = (popstateEvent -> unit[@mel])

external page_x : 'e Dom.event_like -> int = "pageX" [@@mel.get]
external page_y : 'e Dom.event_like -> int = "pageY" [@@mel.get]
external client_x : 'e Dom.event_like -> int = "clientX" [@@mel.get]
external client_y : 'e Dom.event_like -> int = "clientY" [@@mel.get]

external length_computable : Dom.progressEvent -> bool = "lengthComputable" [@@mel.send]
external loaded : Dom.progressEvent -> int = "loaded" [@@mel.send]
external total: Dom.progressEvent -> int = "total" [@@mel.send]

external _target : 'e Dom.event_like -> 'a Dom.eventTarget_like Js.undefined = "target"
[@@mel.get]

let target ev = Js.Undefined.toOption (_target ev)

module Target = struct
    external value : 'a Dom.eventTarget_like -> string = "value" [@@mel.get]

    external _checked : 'a Dom.eventTarget_like -> bool Js.undefined = "checked" [@@mel.get]
    let checked tg = Js.Undefined.toOption (_checked tg)
end

