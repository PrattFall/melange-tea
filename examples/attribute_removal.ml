open Tea.App
open Tea.Html
open Tea.Html.Events
open Tea.Html.Attributes

type model = { selected : string option; languages : string list }
type message = Select of string | Delete

let render_selected = function
  | Some selected ->
      div []
        [
          text ("you selected " ^ selected);
          div [ onClick Delete ] [ text "delete selection" ];
        ]
  | None -> div [] [ text "Nothing selected" ]

let lang l is_selected =
  li
    [
      onClick (Select l);
      (if is_selected then raw "" "data-lang" l else noProp);
      (* TODO: combine style attributes *)
      (if is_selected then style "border" "1px solid black" else style "color" "blue");
    ]
    [ text l ]

let render_languages selected languages =
  let is_selected selected language =
    match selected with Some l -> language == l | None -> false
  in

  let rendered =
    Belt.List.map languages (fun l -> lang l (is_selected selected l))
  in

  ul [] rendered

let update state = function
  | Select lang -> { state with selected = Some lang }
  | Delete -> { state with selected = None }

let view state =
  div []
    [
      render_selected state.selected;
      render_languages state.selected state.languages;
    ]

let main =
  let initialState =
    { selected = Some "Erlang"; languages = [ "Erlang"; "Ocaml"; "Clojure" ] }
  in
  beginnerProgram { model = initialState; update; view }
