open Tea
open Tea.Html
open Tea.Html.Events

type model = { face : int }
type msg = Roll | NewFace of int [@@deriving accessors]

let init = { face = 1 }

let update model = function
  | Roll -> (model, Random.generate newFace (Random.int 1 6))
  | NewFace f -> ({ face = f }, Cmd.none)

let view model =
  div []
    [
      h1 [] [ text (string_of_int model.face) ];
      button [ onClick Roll ] [ text "Roll" ];
    ]
