type 'msg applicationCallbacks = 'msg Vdom.ApplicationCallbacks.t

type 'msg t =
  | NoSub : _ t
  | Batch : 'msg t list -> 'msg t
  | Registration :
      string
      * ('msg applicationCallbacks ref -> unit -> unit)
      * (unit -> unit) option ref
      -> 'msg t
  | Mapper :
      ('msg applicationCallbacks ref -> 'msgB applicationCallbacks ref)
      * 'msgB t
      -> 'msg t

let none = NoSub
let batch subs = Batch subs

let registration key enableCall =
  Registration (key, (fun callbacks -> enableCall !callbacks), ref None)

let map msgMapper sub =
  let func callbacks =
    Vdom.ApplicationCallbacks.wrap_callbacks msgMapper callbacks
  in
  Mapper (func, sub)

let mapFunc func sub = Mapper (func, sub)

let rec run :
    type msgOld msgNew.
    msgOld applicationCallbacks ref ->
    msgNew applicationCallbacks ref ->
    msgOld t ->
    msgNew t ->
    msgNew t =
 fun oldCallbacks newCallbacks oldSub newSub ->
  let rec enable : type msg. msg applicationCallbacks ref -> msg t -> unit =
   fun callbacks -> function
    | NoSub -> ()
    | Batch [] -> ()
    | Batch subs -> List.iter (enable callbacks) subs
    | Mapper (mapper, sub) ->
        let subCallbacks = mapper callbacks in
        enable subCallbacks sub
    | Registration (_key, enCB, diCB) -> diCB := Some (enCB callbacks)
  in
  let rec disable : type msg. msg applicationCallbacks ref -> msg t -> unit =
   fun callbacks -> function
    | NoSub -> ()
    | Batch [] -> ()
    | Batch subs -> List.iter (disable callbacks) subs
    | Mapper (mapper, sub) ->
        let subCallbacks = mapper callbacks in
        disable subCallbacks sub
    | Registration (_key, _enCB, diCB) -> (
        match !diCB with
        | None -> ()
        | Some cb ->
            diCB := None;
            cb ())
  in
  match (oldSub, newSub) with
  | NoSub, NoSub -> newSub
  | ( Registration (oldKey, _oldEnCB, oldDiCB),
      Registration (newKey, _newEnCB, newDiCB) )
    when oldKey = newKey ->
      newDiCB := !oldDiCB;
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
