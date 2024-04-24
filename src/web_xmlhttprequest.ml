type unresolved

type xmlHttpRequestUpload

type event_readystatechange = Web_json.t
type event_abort = Web_json.t
type event_error = Web_json.t
type event_load = Web_json.t
type event_loadstart = Web_json.t
type event_progress = Web_json.t
type event_timeout = Web_json.t
type event_loadend = Web_json.t

type t

external create: unit -> t = "XMLHttpRequest" [@@mel.new]

external abort: unit = "abort" [@@mel.send.pipe: t]

external get_all_response_headers: string Js.null = "getAllResponseHeaders"
    [@@mel.send.pipe: t]

external _open: string -> string -> bool -> string -> string -> unit = "_open"
    [@@mel.send.pipe: t]

external override_mime_type: string -> unit = "overrideMimeType"
    [@@mel.send.pipe: t]

external send: unit = "send" [@@mel.send.pipe: t]

external send_string: string Js.null -> unit = "send__string"
    [@@mel.send.pipe: t]

external send_form_data: Web_formdata.t -> unit = "send__formdata"
    [@@mel.send.pipe: t]

external send_document: Web_document.t -> unit = "send__document"
    [@@mel.send.pipe: t]

external set_request_header: string -> string -> unit = "setRequestHeader"
    [@@mel.send.pipe: t]

type readystatechange = (event_readystatechange -> unit)
external set_onreadystatechange: t -> readystatechange -> unit = "onReadyStateChange" [@@mel.set]
external get_onreadystatechange: t -> readystatechange = "onReadyStateChange" [@@mel.get]
external readyState: t -> int = "readyState" [@@mel.get]
external responseType: t -> string = "responseType" [@@mel.get]
external setResponseType: t -> string -> unit = "responseType" [@@mel.set]
external response: t -> unresolved Js.null = "response" [@@mel.get]
external responseText: t -> string = "responseText" [@@mel.get]
external responseURL: t -> string = "responseURL" [@@mel.get]
external responseXML: t -> Web_document.t Js.null = "responseXML" [@@mel.get]
external status: t -> int = "status" [@@mel.get]
external statusText: t -> string = "statusText" [@@mel.get]
external timeout: t -> float = "timeout" [@@mel.get]
external setTimeout: t -> float -> unit = "timeout" [@@mel.set]
external upload: t -> xmlHttpRequestUpload = "upload" [@@mel.get]
external withCredentials: t -> bool = "withCredentials" [@@mel.get]
external set_withCredentials: t -> bool -> unit = "withCredentials" [@@mel.set]

type on_abort = (event_abort -> unit)
external onabort: t -> on_abort = "onAbort" [@@mel.get]
external set_onabort: t -> on_abort -> unit = "onAbort" [@@mel.set]

type on_error = (event_error -> unit)
external onerror: t -> on_error = "onError" [@@mel.get]
external set_onerror: t -> on_error -> unit = "onError" [@@mel.set]

type on_load = (event_load -> unit)
external onload: t -> on_load = "onLoad" [@@mel.get]
external set_onload: t -> on_load -> unit = "onLoad" [@@mel.set]

type on_load_start = (event_loadstart -> unit)
external onloadstart: t -> on_load_start = "onLoadStart" [@@mel.get]
external set_onloadstart: t -> on_load_start -> unit = "onLoadStart" [@@mel.set]

type on_progress = (event_loadstart -> unit)
external onProgress:     t -> on_progress         = "onProgress" [@@mel.get]
external set_onProgress: t -> on_progress -> unit = "onProgress" [@@mel.set]

type on_timeout = (event_timeout -> unit)
external ontimeout: t -> on_timeout = "onTimeout" [@@mel.get]
external set_ontimeout: t -> on_timeout -> unit = "onTimeout" [@@mel.set]

type on_load_end = (event_loadend -> unit)
external onloadend: t -> on_load_end = "onLoadEnd" [@@mel.get]
external set_onloadend: t -> on_load_end -> unit = "onLoadEnd" [@@mel.set]

type errors =
  | IncompleteResponse
  | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody of string
  | FormDataBody of Web_formdata.t
  | FormListBody of (string * string) list
  | DocumentBody of Web_document.t
  (* | BlobBody of Web_blob.t *)
  (* | ArrayBufferViewBody of Web_arraybuffer_view.t *)

(* Main interface functions *)

let getAllResponseHeaders (x: t) : (string, errors) result =
  match Js.Null.toOption (get_all_response_headers x) with
  | None -> Error IncompleteResponse
  | Some "" -> Error NetworkError
  | Some s -> Ok s

let getAllResponseHeadersAsList (x: t) : ((string * string) list, errors) result =
  let format_value (s: Js.String.t) =
      s
      |> Js.String.split ~sep:"\r\n"
      |> Array.map (Js.String.split ~sep:": ")
      |> Array.map Array.to_seq
      |> Array.map (Seq.take 2)
      |> Array.map Array.of_seq
      |> Array.to_list
      |> List.filter (fun a -> Array.length a == 2)
      |> List.map
        ( function
          | [|key; value|] -> (key, value)
          | _ -> failwith "Cannot happen, already checked length"
        )
  in

  match getAllResponseHeaders x with
  | Error _ as err -> err
  | Ok s -> Ok (format_value s)

let getAllResponseHeadersAsDict (x: t) : (string Map.Make(String).t, errors) result =
  let module StringMap = Map.Make(String) in
  match getAllResponseHeadersAsList x with
  | Error _ as err -> err
  | Ok l ->
    let insert d (k, v) = StringMap.add k v d in
    Ok (List.fold_left insert StringMap.empty l)

let open_ (method': string) (url: string) ?(async=true) ?(user="") ?(password="") x =
  _open method' url async user password x

let send (body: body) (x: t) : unit =
  match body with
  | EmptyBody -> send x
  | EmptyStringBody -> send_string Js.Null.empty x
  | StringBody s -> send_string (Js.Null.return s) x
  | FormDataBody f -> send_form_data f x
  | FormListBody l ->
    let form =
      List.fold_left
        (fun f (key, value) -> let () = Web_formdata.append key value f in f)
        (Web_formdata.create ())
        l in
    send_form_data form x
  | DocumentBody d -> send_document d x
  (* | BlobBody b -> x##send_blob b *)
  (* | ArrayBufferViewBody a -> x##send_arrayBufferView a *)

(* Properties *)

type state =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done

type responseType =
  | StringResponseType
  | ArrayBufferResponseType
  | BlobResponseType
  | DocumentResponseType
  | JsonResponseType
  | TextResponseType
  | RawResponseType of string

type responseBody =
  | NoResponse
  | StringResponse of string
  | ArrayBufferResponse of unit
  | BlobResponse of unit
  | DocumentResponse of Web_document.t
  | JsonResponse of Web_json.t
  | TextResponse of string
  | RawResponse of string * unit

let set_onreadystatechange (cb: event_readystatechange -> unit) (x: t) : unit =
    set_onreadystatechange x cb

let get_onreadystatechange (x: t) : (event_readystatechange -> unit) =
    get_onreadystatechange x

let readyState (x: t) : state =
  match readyState x with
  | 0 -> Unsent
  | 1 -> Opened
  | 2 -> HeadersReceived
  | 3 -> Loading
  | 4 -> Done
  | i -> failwith ("Invalid return from 'readystate' of: " ^ string_of_int i)

let set_responseType (typ: responseType) (x: t) : unit =
  match typ with
  | StringResponseType -> setResponseType x ""
  | ArrayBufferResponseType -> setResponseType x "arraybuffer"
  | BlobResponseType -> setResponseType x "blob"
  | DocumentResponseType -> setResponseType x "document"
  | JsonResponseType -> setResponseType x "json"
  | TextResponseType -> setResponseType x "text"
  | RawResponseType s -> setResponseType x s

let get_responseType (x: t) : responseType =
  match responseType x with
  | "" -> StringResponseType
  | "arraybuffer" -> ArrayBufferResponseType
  | "blob" -> BlobResponseType
  | "document" -> DocumentResponseType
  | "json" -> JsonResponseType
  | "text" -> TextResponseType
  | s -> RawResponseType s

let get_response (x: t) : responseBody =
  match Js.Null.toOption (response x) with
  | None -> NoResponse
  | Some resp ->
    match get_responseType x with
    | StringResponseType -> StringResponse (Obj.magic resp)
    | ArrayBufferResponseType -> ArrayBufferResponse (Obj.magic resp)
    | BlobResponseType -> BlobResponse (Obj.magic resp)
    | DocumentResponseType -> DocumentResponse (Obj.magic resp)
    | JsonResponseType -> JsonResponse (Obj.magic resp)
    | TextResponseType -> TextResponse (Obj.magic resp)
    | RawResponseType s -> RawResponse (s, Obj.magic resp)

let get_responseText (x: t) : string = responseText x

let get_responseURL (x: t) : string = responseURL x

let get_responseXML (x: t) : Web_document.t option =
  Js.Null.toOption (responseXML x)

let get_status (x: t) : int = status x

let get_statusText (x: t) : string = statusText x

let set_timeout (t: float) (x: t) : unit =
    setTimeout x t

let get_timeout (x: t) : float = timeout x

let set_withCredentials (b: bool) (x: t) : unit =
    set_withCredentials x b

let get_withCredentials (x: t) : bool =
    withCredentials x

let set_onabort (cb: event_abort -> unit) (x: t) : unit =
    set_onabort x cb

let get_onabort (x: t) : (event_abort -> unit) = onabort x

let set_onerror (cb: event_error -> unit) (x: t) : unit =
    set_onerror x cb

let get_onerror (x: t) : (event_error -> unit)= onerror x

let set_onload (cb: event_load -> unit) (x: t) : unit = set_onload x cb

let get_onload (x: t) : (event_load -> unit) = onload x

let set_onloadstart (cb: event_loadstart -> unit) (x: t) : unit =
    set_onloadstart x cb

let get_onloadstart (x: t) : (event_loadstart -> unit) = onloadstart x

let set_ontimeout (cb: event_timeout -> unit) (x: t) : unit =
    set_ontimeout x cb

let get_ontimeout (x: t) : (event_timeout -> unit) = ontimeout x

let set_onloadend (cb: event_loadend -> unit) (x: t) : unit =
    set_onloadend x cb

let get_onloadend (x: t) : (event_loadend -> unit) = onloadend x
