(* TODO:  Polyfill window if it is missing, like on node or in native *)

module History = Web_window_history

type timeoutHandlerID = int

external window : Dom.window = "window"

external history : Dom.window -> Dom.history Js.undefined = "history"
[@@mel.get]

external local_storage : Dom.window -> Dom.Storage.t Js.undefined
  = "localStorage"
[@@mel.get]

external location : Dom.window -> Web_location.t = "location" [@@mel.get]

let get_history () = history window
let get_local_storage () = Js.Undefined.toOption (local_storage window)
let get_location () = location window

(* requestAnimationFrame callback is a float timestamp in milliseconds *)
external requestAnimationFrame : Dom.window -> (float -> unit) -> int
  = "requestAnimationFrame"
[@@mel.send]

external cancelAnimationFrame : Dom.window -> int -> unit
  = "cancelAnimationFrame"
[@@mel.send]

external clearTimeout : Dom.window -> timeoutHandlerID -> unit = "clearTimeout"
[@@mel.send]

external setTimeout : Dom.window -> (unit -> unit) -> float -> timeoutHandlerID
  = "setTimeout"
[@@mel.send]

external setInterval : Dom.window -> (unit -> unit) -> float -> timeoutHandlerID
  = "setInterval"
[@@mel.send]

external addEventListener :
  Dom.window -> string -> ('e Dom.event_like -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external removeEventListener :
  Dom.window -> string -> ('e Dom.event_like -> unit) -> unit
  = "removeEventListener"
[@@mel.send]

let add_event_listener event_name cb = addEventListener window event_name cb
let remove_event_listener event_name cb = removeEventListener window event_name cb
let request_animation_frame = requestAnimationFrame window
let cancel_animation_frame = cancelAnimationFrame window
let clear_timeout = clearTimeout window
let set_timeout = setTimeout window
let set_interval = setInterval window

(* Polyfills *)

let requestAnimationFrame_polyfill : unit -> unit =
 fun () ->
  [%mel.raw
    {|
  // requestAnimationFrame polyfill
  (function() {
      var lastTime = 0;
      var vendors = ['ms', 'moz', 'webkit', 'o'];
      for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
          window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
          window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame']
                                     || window[vendors[x]+'CancelRequestAnimationFrame'];
      }

      if (!window.requestAnimationFrame)
          window.requestAnimationFrame = function(callback, element) {
              var currTime = new Date().getTime();
              var timeToCall = Math.max(0, 16 - (currTime - lastTime));
              var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                timeToCall);
              lastTime = currTime + timeToCall;
              return id;
          };

      if (!window.cancelAnimationFrame)
          window.cancelAnimationFrame = function(id) {
              clearTimeout(id);
          };
  }())
  |}]
