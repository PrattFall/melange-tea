type example_key =
  | UnevenProperties
  | CounterDebugBeginner
  | CounterDebugStandard
  | CounterDebugProgram
  | Datasets
  | ButtonUpdateSpan
  | AttributeRemoval
  | Drag
  | OnWithOptions
  | HttpTask

let example_key_to_string = function
  | UnevenProperties -> "Uneven Properties"
  | CounterDebugBeginner -> "Counter Debug (Beginner)"
  | CounterDebugStandard -> "Counter Debug (Standard)"
  | CounterDebugProgram -> "Counter Debug (Program)"
  | Datasets -> "Datasets"
  | ButtonUpdateSpan -> "Button Update Span"
  | AttributeRemoval -> "Attribute Removal"
  | Drag -> "Drag"
  | OnWithOptions -> "On -- With Options"
  | HttpTask -> "HTTP Task"

external innerHtml : Dom.element -> string -> unit = "innerHTML" [@@mel.set]

let elem = Tea.Web.Document.get_element_by_id "content"
let buttons : Dom.element option = Tea.Web.Document.get_element_by_id "buttons"

let uneven_properties () =
  Uneven_properties.main (Js.Nullable.fromOption elem) () |> ignore

let counter_debug_beginner () =
  Counter_debug_beginner.main (Js.Nullable.fromOption elem) () |> ignore

let counter_debug_standard () =
  Counter_debug_standard.main (Js.Nullable.fromOption elem) () |> ignore

let counter_debug_program () =
  Counter_debug_program.main (Js.Nullable.fromOption elem) () |> ignore

let btn_update_span () =
  Button_update_span.main (Js.Nullable.fromOption elem) () |> ignore

let attribute_removal () =
  Attribute_removal.main (Js.Nullable.fromOption elem) () |> ignore

let drag () = Drag.main (Js.Nullable.fromOption elem) () |> ignore

let datasets() = Datasets.main (Js.Nullable.fromOption elem) () |> ignore

let on_with_options () =
  Events_with_options.main (Js.Nullable.fromOption elem) () |> ignore

let http_task () = Http_task.main (Js.Nullable.fromOption elem) () |> ignore

let examples =
  [
    (UnevenProperties, uneven_properties);
    (CounterDebugBeginner, counter_debug_beginner);
    (CounterDebugStandard, counter_debug_standard);
    (CounterDebugProgram, counter_debug_program);
    (Datasets, datasets);
    (ButtonUpdateSpan, btn_update_span);
    (AttributeRemoval, attribute_removal);
    (Drag, drag);
    (OnWithOptions, on_with_options);
    (HttpTask, http_task);
  ]
;;

List.iter
  (fun (key, program) ->
    let new_button = Tea.Web.Document.create_element "button" in
    innerHtml new_button (example_key_to_string key);
    Tea.Web.Node.add_event_listener new_button "click" (fun _ -> program ());

    match buttons with
    | Some btns -> Tea.Web.Node.append_child btns new_button |> ignore
    | None -> ())
  examples
