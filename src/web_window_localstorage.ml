(* type t = *)
(*   < length : int [@mel.get] *)
(*   ; clear : unit -> unit [@mel.meth] *)
(*   ; key : int -> string [@mel.meth] *)
(*   ; getItem : string -> string [@mel.meth] *)
(*   ; removeItem : string -> unit [@mel.meth] *)
(*   ; setItem : string -> string -> unit [@mel.meth] > *)
(*   Js.t *)
(**)
(* let map_storage mapper window = *)
(*     Option.map mapper (Web_window.local_storage window) *)
(**)
(* let length = map_storage (fun storage -> storage##length) *)
(* let clear = map_storage (fun storage -> storage##clear ()) *)
(* let key idx = map_storage (fun storage -> storage##key idx) *)
(* let get_item key = map_storage (fun storage -> storage##getItem key) *)
(* let remove_item key = map_storage (fun storage -> storage##removeItem key) *)
(* let set_item key value = map_storage (fun storage -> storage##setItem key value) *)