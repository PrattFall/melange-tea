let cmd promise tagger =
  let open Vdom in
  Tea_cmd.call (function callbacks ->
      let _ =
        promise
        |> Js.Promise.then_ (function res ->
               (match tagger res with
               | Some msg ->
                   let () = !callbacks.enqueue msg in
                   Js.Promise.resolve ()
               | None -> Js.Promise.resolve ()))
      in
      ())

let result promise msg =
  let open Vdom in
  Tea_cmd.call (function callbacks ->
      let enq result = !callbacks.enqueue (msg result) in
      let _ =
        promise
        |> Js.Promise.then_ (function res ->
               let resolve = enq (Ok res) in
               Js.Promise.resolve resolve)
        |> Js.Promise.catch (function _ ->
               let reject = enq (Error "error in promise") in
               Js.Promise.resolve reject)
      in
      ())
