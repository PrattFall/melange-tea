external length : Dom.history -> int = "length" [@@mel.get]
external back : Dom.history -> unit = "back" [@@mel.send]
external forward : Dom.history -> unit = "forward" [@@mel.send]
external go : Dom.history -> int -> unit = "go" [@@mel.send]
external pushState : Dom.history -> Js.Json.t -> string -> string -> unit = "pushState" [@@mel.send]
external replaceState : Dom.history -> Js.Json.t -> string -> string -> unit = "replaceState" [@@mel.send]
external state : Dom.history -> Js.Json.t = "state" [@@mel.get]

let map_window mapper history =
    Js.Undefined.toOption history
    |> Option.map mapper

let length' = map_window length
let back' = map_window back
let forward' = map_window forward
let go' n = map_window (fun hs -> go hs n)
let push_state' state url = map_window (fun hs -> pushState hs state "" url)
let replace_state' state url = map_window (fun hs -> replaceState hs state "" url)
let state' = map_window state

