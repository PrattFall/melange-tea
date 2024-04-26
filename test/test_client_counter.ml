open Tea.App
open Tea.Html

type msg = Increment | Decrement | Reset | Set of int

let init = 4

let update model = function
  | Increment -> model + 1
  | Decrement -> model - 1
  | Reset -> init
  | Set v -> v

let view_button title msg = div [] [ button [ onClick msg ] [ text title ] ]

let view model =
  div []
    [
      span
        [
          (if model > 10 then
             styles [ ("color", "purple"); ("font-weight", "bold") ]
          else styles [ ("font-weight", "bold"); ("text-decoration", "underline") ]);
        ]
        [ text (string_of_int model) ];
      view_button "Increment" Increment;
      view_button "Decrement" Decrement;
      view_button "Set to 42" (Set 42);
      (if model <> init then view_button "Reset" Reset else noNode);
    ]

let main = beginnerProgram { model = init; update; view }
