open Tea
open Tea.Html
open Tea.Html.Events

type msg = GotResponse of (string, string) result | Req

let gotResponse x = GotResponse x

let update model = function
  | GotResponse (Ok t) -> (t, Cmd.none)
  | GotResponse (Error t) -> (t, Cmd.none)
  | Req ->
      ( model,
        Js.Promise.(
          Fetch.fetch "https://jsonplaceholder.typicode.com/todos/1"
          |> then_ Fetch.Response.text
          |> then_ (fun res ->
                 Ex.LocalStorage.setItem "todo-1" res;
                 Js.Promise.resolve res)
          |. Tea.Promise.result gotResponse) )

let view model =
  div [] [ button [ onClick Req ] [ text "execute" ]; text model ]

let string_of_model = function
  | GotResponse (Ok _) -> "GotResponse Ok"
  | GotResponse (Error _) -> "GotResponse Error"
  | Req -> "Req"

let main =
  Tea.Debug.standardProgram
    {
      init = (fun () -> ("nothing", Cmd.none));
      subscriptions = (fun _ -> Sub.none);
      update;
      view;
    }
    string_of_model
