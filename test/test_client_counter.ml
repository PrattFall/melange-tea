open Tea.App
open Tea.Html

type msg = Increment | Decrement | Reset | Set of int

let init = 4

let update model = function
  | Increment -> model + 1
  | Decrement -> model - 1
  | Reset -> init
  | Set v -> v

let view_button title msg = button [ onClick msg ] [ text title ]

let view model =
  div []
    [
      span [ style "text-weight" "bold" ] [ text (string_of_int model) ];
      br [];
      (if model > 10 then div [] [ text "TOO BIG" ] else noNode);
      view_button "Increment" Increment;
      br [];
      view_button "Decrement" Decrement;
      br [];
      view_button "Set to 42" (Set 42);
      br [];
      (if model <> 0 then view_button "Reset" Reset else noNode);
    ]

let main = beginnerProgram { model = init ; update; view }
