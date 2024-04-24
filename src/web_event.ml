(* type target = <
     value : string Js.undefined [@mel.get];
   > Js.t *)

type 'node t =
  < target : 'node Js.undefined [@mel.get]
  ; keyCode : int [@mel.get]
  ; preventDefault : unit -> unit [@mel.meth]
  ; stopPropagation : unit -> unit [@mel.meth] >
  Js.t

type 'node cb = ('node t -> unit[@mel])
type options = bool
(* false | true (* TODO:  Define a javascript record as another option *) *)

type popstateEvent = < > Js.t
type popstateCb = (popstateEvent -> unit[@mel])
