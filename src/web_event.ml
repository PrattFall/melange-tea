(* type target = <
     value : string Js.undefined [@mel.get];
   > Js.t *)

external target : 'e Dom.event_like -> 'a Dom.node_like Js.undefined = "target"
[@@mel.get]

external keyCode : 'e Dom.event_like -> int = "keyCode" [@@mel.get]

external preventDefault : 'e Dom.event_like -> unit = "preventDefault"
[@@mel.send]

external stopPropagation : 'e Dom.event_like -> unit = "stopPropagation"
[@@mel.send]

type 'e callback = 'e Dom.event_like -> unit

type options = bool

let emptyCallback: 'e callback = (fun  _ev -> ())

(* false | true (* TODO:  Define a javascript record as another option *) *)

type popstateEvent = < > Js.t
type popstateCb = (popstateEvent -> unit[@mel])
