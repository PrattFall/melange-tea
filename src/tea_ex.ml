let render_event ?(key = "") msg =
  let open Vdom.ApplicationCallbacks in
  let enableCall callbacks =
    callbacks.on (AddRenderMsg msg);
    fun () -> callbacks.on (RemoveRenderMsg msg)
  in

  Tea_sub.registration key enableCall

module LocalStorage = struct
  let inner mapper =
    match Web.Window.get_local_storage () with
    | None -> Js.Exn.raiseError "localStorage is not available"
    | Some value -> mapper value

  let length = inner Dom.Storage.length
  let clear = inner Dom.Storage.clear
  let key idx = inner (Dom.Storage.key idx)
  let getItem key = inner (Dom.Storage.getItem key)
  let removeItem key = inner (Dom.Storage.removeItem key)
  let setItem key value = inner (Dom.Storage.setItem key value)
end
