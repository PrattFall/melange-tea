let render_event ?(key = "") msg =
  let open Vdom.ApplicationCallbacks in
  let enableCall callbacks =
    callbacks.on (AddRenderMsg msg);
    fun () -> callbacks.on (RemoveRenderMsg msg)
  in

  Tea_sub.registration key enableCall

module LocalStorage = struct
  open Tea_task

  let inner mapper =
    nativeBinding (fun cb ->
        match Web.Window.get_local_storage () with
        | None -> cb (Error "localStorage is not available")
        | Some value -> cb (Ok (mapper value)))

  let length = inner Dom.Storage.length
  let clear = inner Dom.Storage.clear
  let clearCmd () = Tea_task.attemptOpt (fun _ -> None) clear
  let key idx = inner (Dom.Storage.key idx)
  let getItem key = inner (Dom.Storage.getItem key)
  let removeItem key = inner (Dom.Storage.removeItem key)
  let removeItemCmd key = Tea_task.attemptOpt (fun _ -> None) (removeItem key)
  let setItem key value = inner (Dom.Storage.setItem key value)

  let setItemCmd key value =
    Tea_task.attemptOpt (fun _ -> None) (setItem key value)
end
