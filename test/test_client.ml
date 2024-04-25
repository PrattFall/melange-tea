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

type document

external document : document = "document"

external get_by_id : document -> string -> Dom.element Js.nullable = "getElementById"
[@@mel.send]

external children : Dom.element -> Dom.nodeList = "childNodes" [@@mel.get]

let counter =
  let open Test_client_counter in
  main

let counter_debug_beginner =
  let open Test_client_counter_debug_beginner in
  main

let counter_debug_standard =
  let open Test_client_counter_debug_standard in
  main

let counter_debug_program =
  let open Test_client_counter_debug_program in
  main

let btn_update_span =
  let open Test_client_btn_update_span in
  main

let attribute_removal =
  let open Test_client_attribute_removal in
  main

let drag =
  let open Test_client_drag in
  main

let on_with_options =
  let open Test_client_on_with_options in
  main

let http_task =
  let open Test_client_http_task in
  main

let elem = get_by_id document "content"
let buttons = get_by_id document "buttons";;

counter elem () |> ignore;;

let all_examples =
  [
    Counter;
    CounterDebugBeginner;
    CounterDebugStandard;
    CounterDebugProgram;
    ButtonUpdateSpan;
    AttributeRemoval;
    Drag;
    OnWithOptions;
    HttpTask;
  ]
;;
