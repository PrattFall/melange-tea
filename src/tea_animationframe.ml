type t = { time : Tea_time.t; delta : Tea_time.t }

let every ?(key = "") tagger =
  let open Vdom.ApplicationCallbacks in

  let enableCall callbacks =
    (* let () = Js.log ("rAF", "enable") in *)
    let last_time = ref (Web.Date.now ()) in
    let id = ref None in

    let rec on_frame _time =
      let time = Web.Date.now () in
      match !id with
      | None -> ()
      | Some _i ->
          let ret =
            {
              time;
              delta = (if time < !last_time then 0.0 else time -. !last_time);
            }
          in
          last_time := time;
          callbacks.enqueue (tagger ret);
          Option.iter
            (fun _stillActive ->
              id := Some (Web.Window.request_animation_frame on_frame))
            !id
    in

    id := Some (Web.Window.request_animation_frame on_frame);

    fun () ->
      match !id with
      | None -> ()
      | Some i ->
          (* let () = Js.log ("rAF", "disable") in *)
          Web.Window.cancel_animation_frame i;
          id := None;
  in
  Tea_sub.registration key enableCall

let times ?(key = "") tagger = every (fun ev -> tagger ~key ev.time)
let diffs ?(key = "") tagger = every (fun ev -> tagger ~key ev.delta)
