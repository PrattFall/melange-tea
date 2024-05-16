type ('flags, 'model, 'msg) navigationProgram = {
  init : 'flags -> Web.Location.location -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.Node.t;
  subscriptions : 'model -> 'msg Tea_sub.t;
  shutdown : 'model -> 'msg Tea_cmd.t;
}

let getLocation () = Web.Location.asRecord (Web.Document.get_location ())
let notifier : (Web.Location.location -> unit) option ref = ref None

let notifyUrlChange () =
  Option.iter
    (fun cb ->
      let location = getLocation () in
      cb location)
    !notifier

let subscribe tagger =
  let open Vdom.ApplicationCallbacks in

  let enableCall callbacks =
    let notifyHandler location = callbacks.enqueue (tagger location) in

    notifier := Some notifyHandler;

    let handler : 'e Dom.event_like -> unit =
     fun _event -> notifyUrlChange ()
    in

    Web.Window.add_event_listener "popstate" handler;

    fun () -> Web.Window.remove_event_listener "popstate" handler
  in
  Tea_sub.registration "navigation" enableCall

let replaceState url =
  ignore
    (Web.Window.History.replace_state' (Js.Json.parseExn "{}") url
       (Web.Window.get_history ()))

let pushState url =
  ignore
    (Web.Window.History.push_state' (Js.Json.parseExn "{}") url
       (Web.Window.get_history ()))

let modifyUrl url =
  Tea_cmd.call (fun _enqueue ->
      replaceState url;
      notifyUrlChange ();
      ())

let newUrl url =
  Tea_cmd.call (fun _enqueue ->
      pushState url;
      notifyUrlChange ();
      ())

let go step =
  Tea_cmd.call (fun _enqueue ->
      ignore (Web.Window.History.go' step (Web.Window.get_history ()));
      notifyUrlChange ();
      ())

let back step = go (-step)
let forward step = go step

let navigationProgram locationToMessage stuff =
  let init flag = stuff.init flag (getLocation ()) in

  let subscriptions model =
    Tea_sub.batch [ subscribe locationToMessage; stuff.subscriptions model ]
  in

  Tea_app.program
    {
      init;
      update = stuff.update;
      view = stuff.view;
      subscriptions;
      shutdown = stuff.shutdown;
    }
