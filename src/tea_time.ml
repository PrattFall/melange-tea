type t = float

let every ~key interval tagger =
  let open Vdom.ApplicationCallbacks in
  let enableCall callbacks =
    let id =
      Web.Window.set_interval
        (fun () -> callbacks.enqueue (tagger (Web.Date.now ())))
        interval
    in
    fun () -> Web.Window.clear_timeout id
  in
  Tea_sub.registration key enableCall

let delay msTime msg =
  Tea_cmd.call (fun callbacks ->
      let _unhandledID =
        Web.Window.set_timeout
          (fun () ->
            let open Vdom.ApplicationCallbacks in
            !callbacks.enqueue msg)
          msTime
      in
      ())

(* Generic Helpers *)

let milliseconds (m : float) = m
let seconds s = 1000.0 *. s
let minutes m = 60.0 *. seconds m
let hours h = 60.0 *. minutes h
