include Js.Json

type nothingYet

external stringify : 't -> nothingYet Js.null -> int -> string
  = "JSON.stringify"

let string_of_json ?(indent = 2) value =
  match Js.Undefined.toOption value with
  | None -> "undefined"
  | Some v -> ( try stringify v Js.Null.empty indent with _ -> "")

let of_type (type a) (_v : a kind) (x : a) : t = Obj.magic x

let null : Js.Types.null_val = Obj.magic Js.null
