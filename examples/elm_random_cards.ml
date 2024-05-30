open Tea
open Tea.Html
open Tea.Html.Events

type card =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type model = { card : card }
type msg = Draw | NewCard of card [@@deriving accessors]

let init = { card = Three }

let cardGenerator =
  Random.uniform
    [|
      Ace;
      Two;
      Three;
      Four;
      Five;
      Six;
      Seven;
      Eight;
      Nine;
      Ten;
      Jack;
      Queen;
      King;
    |]

let update model = function
  | Draw -> (model, Random.generate newCard cardGenerator)
  | NewCard c -> ({ card = c }, Cmd.none)

let view_card = function
  | Ace -> "Ace"
  | Two -> "Two"
  | Three -> "Three"
  | Four -> "Four"
  | Five -> "Five"
  | Six -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine -> "Nine"
  | Ten -> "Ten"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"

let view model =
  div []
    [
      div [] [ text "Disclaimer: Either Melange or OCaml doesn't currently seem to handle unicode well" ];
      button [ onClick Draw ] [ text "Draw" ];
      div [ ] [ text (view_card model.card) ];
    ]
