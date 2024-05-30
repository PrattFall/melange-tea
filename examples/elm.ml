open Tea
open Tea.App
open Tea.Html
open Tea.Html.Events

type example = Hello | Groceries | Shapes | TextFields | Forms | RandomNumbers | RandomCards

type msg =
  | ChangeExample of example
  | TextFieldsMsg of Elm_text_fields.msg
  | FormsMsg of Elm_forms.msg
  | RandomNumbersMsg of Elm_random_numbers.msg
  | RandomCardsMsg of Elm_random_cards.msg
[@@deriving accessors]

type model = {
  example : example;
  text_fields_model : Elm_text_fields.model;
  forms_model : Elm_forms.model;
  random_numbers_model : Elm_random_numbers.model;
  random_cards_model : Elm_random_cards.model;
}

let init () =
  ( {
      example = Hello;
      text_fields_model = Elm_text_fields.init;
      forms_model = Elm_forms.init;
      random_numbers_model = Elm_random_numbers.init;
      random_cards_model = Elm_random_cards.init;
    },
    Tea.Cmd.none )

let update model = function
  | ChangeExample e -> ({ model with example = e }, Cmd.none)
  | TextFieldsMsg tmsg ->
      ( {
          model with
          text_fields_model =
            Elm_text_fields.update model.text_fields_model tmsg;
        },
        Cmd.none )
  | FormsMsg fmsg ->
      ( { model with forms_model = Elm_forms.update model.forms_model fmsg },
        Cmd.none )
  | RandomNumbersMsg rmsg ->
      let updated, cmd =
        Elm_random_numbers.update model.random_numbers_model rmsg
      in
      ({ model with random_numbers_model = updated }, Cmd.map randomNumbersMsg cmd)
  | RandomCardsMsg rmsg ->
      let updated, cmd =
        Elm_random_cards.update model.random_cards_model rmsg
      in
      ({ model with random_cards_model = updated }, Cmd.map randomCardsMsg cmd)

let view_example model =
  match model.example with
  | Hello -> Hello.view ()
  | Groceries -> Groceries.view ()
  | Shapes -> Shapes.view ()
  | TextFields ->
      Elm_text_fields.view model.text_fields_model |> map textFieldsMsg
  | Forms -> Elm_forms.view model.forms_model |> map formsMsg
  | RandomNumbers ->
      Elm_random_numbers.view model.random_numbers_model |> map randomNumbersMsg
  | RandomCards ->
      Elm_random_cards.view model.random_cards_model |> map randomCardsMsg

let view model =
  div []
    [
      ul []
        [
          li [ onClick (ChangeExample Hello) ] [ text "Hello" ];
          li [ onClick (ChangeExample Groceries) ] [ text "Groceries" ];
          li [ onClick (ChangeExample Shapes) ] [ text "Shapes" ];
          li [ onClick (ChangeExample TextFields) ] [ text "Text Fields" ];
          li [ onClick (ChangeExample Forms) ] [ text "Forms" ];
          li [ onClick (ChangeExample RandomNumbers) ] [ text "Random Numbers" ];
          li [ onClick (ChangeExample RandomCards) ] [ text "Random Cards" ];
        ];
      view_example model;
    ]

let subscriptions _model = Sub.none
let main = standardProgram { init; update; view; subscriptions }
