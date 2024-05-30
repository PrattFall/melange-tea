open Tea.Html
open Tea.Html.Attributes
open Tea.Html.Events

type model = { name : string; password : string; password_again : string }
type msg = Name of string | Password of string | PasswordAgain of string [@@deriving accessors]

let init = { name = ""; password = ""; password_again = "" }

let update model = function
  | Name n -> { model with name = n }
  | Password p -> { model with password = p }
  | PasswordAgain p -> { model with password_again = p }

let view_input typ p v toMsg =
    input' [ type' typ; placeholder p; defaultValue v; onInput toMsg ] []

let view_validation model =
    if model.password = model.password_again then
        div [ style "color" "green" ] [ text "OK" ]
    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]

let view model =
  div []
    [
        view_input "text" "Name" model.name name;
        view_input "password" "Password" model.password password;
        view_input "password" "Re-enter Password" model.password_again passwordAgain;
        view_validation model;
    ]
