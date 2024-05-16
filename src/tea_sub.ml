module Callbacks = Vdom.ApplicationCallbacks

module Registration = struct
  type 'msg t = {
    key : string;
    enable_callback : 'msg Callbacks.t ref -> unit -> unit;
    disable_callback : (unit -> unit) option ref;
  }

  let make key enable_callback =
    {
      key;
      enable_callback = (fun callbacks -> enable_callback !callbacks);
      disable_callback = ref None;
    }

  let enable callbacks r =
    r.disable_callback := Some (r.enable_callback callbacks)

  let disable r = r.disable_callback := None
end

type 'msg t =
  | NoSub : _ t
  | Batch : 'msg t list -> 'msg t
  | Registration : 'msg Registration.t -> 'msg t
  | Mapper : ('msg Callbacks.t ref -> 'msgB Callbacks.t ref) * 'msgB t -> 'msg t

let none = NoSub
let batch subs = Batch subs

let registration key enableCall =
  Registration (Registration.make key enableCall)

let map msgMapper sub =
  let func callbacks = Callbacks.wrap_callbacks msgMapper callbacks in
  Mapper (func, sub)

let mapFunc func sub = Mapper (func, sub)

let rec run :
    type msgOld msgNew.
    msgOld Callbacks.t ref ->
    msgNew Callbacks.t ref ->
    msgOld t ->
    msgNew t ->
    msgNew t =
 fun oldCallbacks newCallbacks oldSub newSub ->
  let rec enable : type msg. msg Callbacks.t ref -> msg t -> unit =
   fun callbacks -> function
    | NoSub | Batch [] -> ()
    | Batch subs -> List.iter (enable callbacks) subs
    | Mapper (mapper, sub) -> enable (mapper callbacks) sub
    | Registration r -> Registration.enable callbacks r
  in

  let rec disable : type msg. msg Callbacks.t ref -> msg t -> unit =
   fun callbacks -> function
    | NoSub | Batch [] -> ()
    | Batch subs -> List.iter (disable callbacks) subs
    | Mapper (mapper, sub) ->
        let subCallbacks = mapper callbacks in
        disable subCallbacks sub
    | Registration r -> (
        match !(r.disable_callback) with
        | None -> ()
        | Some cb ->
            Registration.disable r;
            cb ())
  in

  match (oldSub, newSub) with
  | NoSub, NoSub -> newSub
  | Registration oldR, Registration newR when oldR.key = newR.key ->
      newR.disable_callback := !(oldR.disable_callback);
      newSub
  | Mapper (oldMapper, oldSubSub), Mapper (newMapper, newSubSub) ->
      let olderCallbacks = oldMapper oldCallbacks in
      let newerCallbacks = newMapper newCallbacks in
      ignore (run olderCallbacks newerCallbacks oldSubSub newSubSub);
      newSub
  | Batch oldSubs, Batch newSubs ->
      let rec aux oldList newList =
        match (oldList, newList) with
        | [], [] -> ()
        | [], newSubSub :: newRest ->
            enable newCallbacks newSubSub;
            aux [] newRest
        | oldSubSub :: oldRest, [] ->
            disable oldCallbacks oldSubSub;
            aux oldRest []
        | oldSubSub :: oldRest, newSubSub :: newRest ->
            ignore (run oldCallbacks newCallbacks oldSubSub newSubSub);
            aux oldRest newRest
      in
      aux oldSubs newSubs;
      newSub
  | oldS, newS ->
      disable oldCallbacks oldS;
      enable newCallbacks newS;
      newSub
