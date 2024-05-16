type response_status = { code : int; message : string }
type requestBody = Web.XMLHttpRequest.body
type bodyType = Web.XMLHttpRequest.responseType
type responseBody = Web.XMLHttpRequest.responseBody

type response = {
  url : string;
  status : response_status;
  headers : string Map.Make(String).t;
  body : responseBody;
}

type 'parsedata error =
  | BadUrl of string
  | Timeout
  | NetworkError
  | Aborted
  | BadStatus of response
  | BadPayload of 'parsedata * response

let string_of_error = function
  | BadUrl url -> "Bad Url: " ^ url
  | Timeout -> "Timeout"
  | NetworkError -> "Unknown network error"
  | Aborted -> "Request aborted"
  | BadStatus resp -> "Bad Status: " ^ resp.url
  | BadPayload (_customData, resp) -> "Bad Payload: " ^ resp.url

type header = Header of string * string
type 'res expect = Expect of bodyType * (response -> ('res, string) result)

module RequestEvents = struct
  type 'msg t = {
    onreadystatechange :
      ('msg Vdom.ApplicationCallbacks.t ref ->
      Web.XMLHttpRequest.event_readystatechange ->
      unit)
      option;
    onprogress :
      ('msg Vdom.ApplicationCallbacks.t ref ->
      Web.XMLHttpRequest.event_progress ->
      unit)
      option;
  }

  let empty = { onreadystatechange = None; onprogress = None }
end

type 'res rawRequest = {
  method' : string;
  headers : header list;
  url : string;
  body : requestBody;
  expect : 'res expect;
  timeout : Tea_time.t option;
  withCredentials : bool;
}

type ('msg, 'res) request =
  | Request of 'res rawRequest * 'msg RequestEvents.t option

let expectStringResponse func =
  let open Web.XMLHttpRequest in
  Expect
    ( TextResponseType,
      fun { body; _ } ->
        match body with
        | TextResponse s -> func s
        | _ -> Error "Non-text response returned" )

let expectString = expectStringResponse (fun resString -> Ok resString)
let request rawRequest = Request (rawRequest, None)

let getString url =
  request
    {
      method' = "GET";
      headers = [];
      url;
      body = Web.XMLHttpRequest.EmptyBody;
      expect = expectString;
      timeout = None;
      withCredentials = false;
    }

let toTask (Request (request, _maybeEvents)) =
  let module StringMap = Map.Make (String) in
  let { method'; headers; url; body; expect; timeout; withCredentials } =
    request
  in
  let (Expect (typ, responseToResult)) = expect in
  Tea_task.nativeBinding (fun cb ->
      let enqRes result _ev = cb result in
      let enqResError result = enqRes (Error result) in
      let enqResOk result = enqRes (Ok result) in
      let xhr = Web.XMLHttpRequest.create () in
      let setEvent ev cb = ev cb xhr in
      setEvent Web.XMLHttpRequest.set_onerror (enqResError NetworkError);
      setEvent Web.XMLHttpRequest.set_ontimeout (enqResError Timeout);
      setEvent Web.XMLHttpRequest.set_onabort (enqResError Aborted);
      let () =
        setEvent Web.XMLHttpRequest.set_onload (fun _ev ->
            let open Web.XMLHttpRequest in
            let headers =
              match getAllResponseHeadersAsDict xhr with
              | Error _e -> StringMap.empty
              | Ok headers -> headers
            in
            let response =
              {
                status = { code = get_status xhr; message = get_statusText xhr };
                headers;
                url = get_responseURL xhr;
                body = get_response xhr;
              }
            in
            if response.status.code < 200 || 300 <= response.status.code then
              enqResError (BadStatus response) ()
            else
              match responseToResult response with
              | Error error -> enqResError (BadPayload (error, response)) ()
              | Ok result -> enqResOk result ())
      in
      let () =
        try Web.XMLHttpRequest.open_ method' url xhr
        with _ -> enqResError (BadUrl url) ()
      in
      let () =
        let setHeader (Header (k, v)) =
          Web.XMLHttpRequest.set_request_header k v xhr
        in
        List.iter setHeader headers;
        Web.XMLHttpRequest.set_responseType typ xhr;
        let () =
          match timeout with
          | None -> ()
          | Some t -> Web.XMLHttpRequest.set_timeout t xhr
        in
        Web.XMLHttpRequest.set_withCredentials withCredentials xhr
      in
      Web.XMLHttpRequest.send body xhr)

let send resultToMessage (Request (request, maybeEvents)) =
  let module StringMap = Map.Make (String) in
  let { method'; headers; url; body; expect; timeout; withCredentials } =
    request
  in
  let (Expect (typ, responseToResult)) = expect in

  Tea_cmd.call (fun callbacks ->
      let enqRes result _ev =
        let open Vdom.ApplicationCallbacks in
        !callbacks.enqueue (resultToMessage result)
      in
      let enqResError result = enqRes (Error result) in
      let enqResOk result = enqRes (Ok result) in
      let xhr = Web.XMLHttpRequest.create () in
      let setEvent ev cb = ev cb xhr in
      let () =
        match maybeEvents with
        | None -> ()
        | Some { onprogress; onreadystatechange } ->
            let open Web.XMLHttpRequest in
            Js.log onprogress;
            let mayCB thenDo = function
              | None -> ()
              | Some v -> thenDo (v callbacks)
            in
            mayCB (setEvent set_onreadystatechange) onreadystatechange
        (* let () = mayCB (setEvent set_onProgress) onprogress in *)
      in
      setEvent Web.XMLHttpRequest.set_onerror (enqResError NetworkError);
      setEvent Web.XMLHttpRequest.set_ontimeout (enqResError Timeout);
      setEvent Web.XMLHttpRequest.set_onabort (enqResError Aborted);
      let () =
        setEvent Web.XMLHttpRequest.set_onload (fun _ev ->
            let open Web.XMLHttpRequest in
            let headers =
              match getAllResponseHeadersAsDict xhr with
              | Error _e -> StringMap.empty
              | Ok headers -> headers
            in
            let response =
              {
                status = { code = get_status xhr; message = get_statusText xhr };
                headers;
                url = get_responseURL xhr;
                body = get_response xhr;
              }
            in
            if response.status.code < 200 || 300 <= response.status.code then
              enqResError (BadStatus response) ()
            else
              match responseToResult response with
              | Error error -> enqResError (BadPayload (error, response)) ()
              | Ok result -> enqResOk result ())
      in
      let () =
        try Web.XMLHttpRequest.open_ method' url xhr
        with _ -> enqResError (BadUrl url) ()
      in
      let () =
        let setHeader (Header (k, v)) =
          Web.XMLHttpRequest.set_request_header k v xhr
        in
        List.iter setHeader headers;
        Web.XMLHttpRequest.set_responseType typ xhr;
        let () =
          match timeout with
          | None -> ()
          | Some t -> Web.XMLHttpRequest.set_timeout t xhr
        in
        Web.XMLHttpRequest.set_withCredentials withCredentials xhr
      in
      Web.XMLHttpRequest.send body xhr)

external encodeURIComponent : string -> string = "encodeURIComponent" [@@bs.val]

let encodeUri str = encodeURIComponent str

external decodeURIComponent : string -> string = "decodeURIComponent" [@@bs.val]

let decodeUri str = try Some (decodeURIComponent str) with _ -> None

module Progress = struct
  type t = { bytes : int; bytesExpected : int }

  let emptyProgress = { bytes = 0; bytesExpected = 0 }

  (* Yeah this does not follow the original API, but that original
     API is... not extensible...  Instead, we have generic event
     listener support here so no need to constrain the API.
     Might still want to make a subscription variant though... *)
  let track toMessage (Request (request, events)) =
    let onprogress =
      Some
        (fun callbacks ev ->
          let open Vdom.ApplicationCallbacks in
          if Web_event.length_computable ev then
            let value =
              {
                bytes = Web_event.loaded ev;
                bytesExpected = Web_event.total ev;
              }
            in
            !callbacks.enqueue (toMessage value))
    in
    let events =
      match events with None -> RequestEvents.empty | Some e -> e
    in
    Request (request, Some { events with onprogress })
end
