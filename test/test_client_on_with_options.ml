open Tea.App
open Tea.Html
open Tea.Html.Attributes
open Tea.Html.Events

type msg = Click | Set_value of int

let update model = function Click -> model + 1 | Set_value n -> n
let set_value v = Set_value v

let view model =
  div []
    (List.map
       (fun e -> div [] [ e ])
       [
         text (string_of_int model);
         button [ onClick Click ] [ text "onClick" ];
         button [ on "click" (send Click) ] [ text "on \"click\"" ];
         a [ href "https://www.google.com" ] [ text "a normal link" ];
         a
           [
             href "https://www.google.com";
             on ~preventDefault:true "click" (send Click);
           ]
           [ text "a link with prevent default" ];
         button
           [
             on "click" (fun e ->
                 Ok (set_value (Tea.Web.Event.client_x e)));
           ]
           [ text "on \"click\", use clientX value" ];
         input'
           [
             type' "text";
             on "input" (fun e ->
                 e |> targetValue
                 |> Option.map (fun v -> v |> int_of_string |> set_value)
                 |> sendOption);
           ]
           [];
       ])

let main = beginnerProgram { model = 0; update; view }
