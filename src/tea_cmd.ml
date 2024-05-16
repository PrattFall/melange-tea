module Callbacks = Vdom.ApplicationCallbacks

type 'msg t =
  | NoCmd
  | Mapper : ('msg Callbacks.t ref -> 'msgB Callbacks.t ref) * 'msgB t -> 'msg t
  | Batch : 'msg t list -> 'msg t
  | EnqueueCall : ('msg Callbacks.t ref -> unit) -> 'msg t

let none = NoCmd
let batch cmds = Batch cmds
let call call = EnqueueCall call
let fnMsg fnMsg = EnqueueCall (fun callbacks -> !callbacks.enqueue (fnMsg ()))
let msg msg = EnqueueCall (fun callbacks -> !callbacks.enqueue msg)

let rec run : type msg. msg Callbacks.t ref -> msg t -> unit =
 fun callbacks -> function
  | NoCmd -> ()
  | Mapper (mapper, cmd) -> run (mapper callbacks) cmd
  | Batch cmds -> List.fold_left (fun () cmd -> run callbacks cmd) () cmds
  | EnqueueCall cb -> cb callbacks

let map : type a b. (a -> b) -> a t -> b t =
 fun func cmd ->
  let mapper = Vdom.ApplicationCallbacks.wrap_callbacks func in
  Mapper (mapper, cmd)
