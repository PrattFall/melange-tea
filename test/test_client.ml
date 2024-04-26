type example_key =
  | Counter
  | CounterDebugBeginner
  | CounterDebugStandard
  | CounterDebugProgram
  | ButtonUpdateSpan
  | AttributeRemoval
  | Drag
  | OnWithOptions
  | HttpTask

let example_key_to_string = function
  | Counter -> "Counter"
  | CounterDebugBeginner -> "Counter Debug (Beginner)"
  | CounterDebugStandard -> "Counter Debug (Standard)"
  | CounterDebugProgram -> "Counter Debug (Program)"
  | ButtonUpdateSpan -> "Button Update Span"
  | AttributeRemoval -> "Attribute Removal"
  | Drag -> "Drag"
  | OnWithOptions -> "On -- With Options"
  | HttpTask -> "HTTP Task"

external innerHtml : Dom.element -> string -> unit = "innerHTML" [@@mel.set]

let elem = Web.Document.get_element_by_id "content"
let buttons : Dom.element option = Web.Document.get_element_by_id "buttons"

let counter () =
  Test_client_counter.main (Js.Nullable.fromOption elem) () |> ignore

let counter_debug_beginner () =
  Test_client_counter_debug_beginner.main (Js.Nullable.fromOption elem) ()
  |> ignore

let counter_debug_standard () =
  Test_client_counter_debug_standard.main (Js.Nullable.fromOption elem) ()
  |> ignore

let counter_debug_program () =
  Test_client_counter_debug_program.main (Js.Nullable.fromOption elem) ()
  |> ignore

let btn_update_span () =
  Test_client_btn_update_span.main (Js.Nullable.fromOption elem) () |> ignore

let attribute_removal () =
  Test_client_attribute_removal.main (Js.Nullable.fromOption elem) () |> ignore

let drag () = Test_client_drag.main (Js.Nullable.fromOption elem) () |> ignore

let on_with_options () =
  Test_client_on_with_options.main (Js.Nullable.fromOption elem) () |> ignore

let http_task () =
  Test_client_http_task.main (Js.Nullable.fromOption elem) () |> ignore

let examples =
  [
    (Counter, counter);
    (CounterDebugBeginner, counter_debug_beginner);
    (CounterDebugStandard, counter_debug_standard);
    (CounterDebugProgram, counter_debug_program);
    (ButtonUpdateSpan, btn_update_span);
    (AttributeRemoval, attribute_removal);
    (Drag, drag);
    (OnWithOptions, on_with_options);
    (HttpTask, http_task);
  ]
;;

List.iter
  (fun (key, program) ->
    let new_button = Web.Document.create_element "button" in
    innerHtml new_button (example_key_to_string key);
    Web.Node.add_event_listener new_button "click" (fun _ -> program ());

    match buttons with
    | Some btns -> Web.Node.append_child btns new_button |> ignore
    | None -> ())
  examples

