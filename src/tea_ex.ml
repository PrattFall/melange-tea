let render_event ?(key = "") msg =
  let open Vdom in
  let enableCall callbacks =
    let () = callbacks.on (AddRenderMsg msg [@explicit_arity]) in
    fun () -> callbacks.on (RemoveRenderMsg msg [@explicit_arity])
  in
  Tea_sub.registration key enableCall

module LocalStorage = struct
  open Tea_task

  let inner mapper =
    nativeBinding (fun cb ->
        match Web.Window.get_local_storage () with
        | None -> cb (Error "localStorage is not available" [@explicit_arity])
        | ((Some value) [@explicit_arity]) ->
            cb (Ok (mapper value) [@explicit_arity]))

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
