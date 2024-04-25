type t = { time : Tea_time.t; delta : Tea_time.t }

let every ?(key = "") tagger =
  let open Vdom in
  let enableCall callbacks =
    (* let () = Js.log ("rAF", "enable") in *)
    let lastTime = ref (Web.Date.now ()) in
    let id = ref None in
    let rec onFrame _time =
      let time = Web.Date.now () in
      match !id with
      | None -> ()
      | Some _i -> (
          let ret =
            {
              time;
              delta = (if time < !lastTime then 0.0 else time -. !lastTime);
            }
          in
          lastTime := time;
          callbacks.enqueue (tagger ret);
          match !id with
          | None -> ()
          | Some _stillActive ->
              let () = id := Some (Web.Window.request_animation_frame onFrame) in
              ())
    in
    let () = id := Some (Web.Window.request_animation_frame onFrame) in
    fun () ->
      match !id with
      | None -> ()
      | Some i ->
          (* let () = Js.log ("rAF", "disable") in *)
          let () = Web.Window.cancel_animation_frame i in
          let () = id := None in
          ()
  in
  Tea_sub.registration key enableCall

let times ?(key = "") tagger = every (fun ev -> tagger ~key ev.time)
let diffs ?(key = "") tagger = every (fun ev -> tagger ~key ev.delta)
