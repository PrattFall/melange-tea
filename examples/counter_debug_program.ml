open Tea.App
open Tea.Html
open Tea.Html.Attributes
open Tea.Html.Events

type msg = Increment | Decrement | Reset | Set of int

let string_of_msg = function
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Reset -> "Reset"
  | Set _ -> "Set"

let init () = (4, Tea.Cmd.none)
let subscriptions _ = Tea.Sub.none

let update model msg =
    let updated = match msg with
      | Increment -> model + 1
      | Decrement -> model - 1
      | Reset -> 0
      | Set v -> v
    in

    (updated, Tea.Cmd.none)

let view_button title msg = button [ onClick msg ] [ text title ]

let view model =
  div []
    [
      span [ style "text-weight" "bold" ] [ text (string_of_int model) ];
      br [];
      view_button "Increment" (if model >= 3 then Decrement else Increment);
      br [];
      view_button "Decrement" Decrement;
      br [];
      view_button "Set to 42" (Set 42);
      br [];
      (if model <> 0 then view_button "Reset" Reset else noNode);
    ]

let main =
  Tea.Debug.program
    {
      init;
      update;
      view;
      subscriptions;
      shutdown = (fun _model -> Tea.Cmd.none);
    }
    string_of_msg
