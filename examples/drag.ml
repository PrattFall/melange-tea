open Tea
open Tea.App
open Tea.Html
open Tea.Html.Events
open Tea.Html.Attributes
open Tea.Mouse

type msg = DragStart of position | DragAt of position | DragEnd of position
[@@deriving accessors]

type drag = { start : position; current : position }
type model = { position : position; drag : drag option }

let init () = ({ position = { x = 200; y = 200 }; drag = None }, Cmd.none)

let getPosition { position; drag } =
  match drag with
  | None -> position
  | Some { start; current } ->
      {
        x = position.x + current.x - start.x;
        y = position.y + current.y - start.y;
      }

let updateHelp model = function
  | DragStart xy ->
      { position = model.position; drag = Some { start = xy; current = xy } }
  | DragAt xy ->
      {
        position = model.position;
        drag = Option.map (fun drag -> { drag with current = xy }) model.drag;
      }
  | DragEnd _ -> { position = getPosition model; drag = None }

let update model msg = (updateHelp model msg, Cmd.none)
let dragStart xy = DragStart xy
let dragAt xy = DragAt xy
let dragEnd xy = DragEnd xy

let subscriptions model =
  match model.drag with
  | None -> Sub.none
  | Some _ -> Sub.batch [ Mouse.moves dragAt; Mouse.ups dragEnd ]

let px number = string_of_int number ^ "px"

let onMouseDown =
  onCB "mousedown" "" (fun ev ->
      ev |> Mouse.position |> dragStart |> Option.some)

let view model =
  let realPosition = getPosition model in
  div
    [
      onMouseDown;
      styles
        [
          ("background-color", "#3C8D2F");
          ("cursor", "move");
          ("width", "100px");
          ("height", "100px");
          ("border-radius", "4px");
          ("position", "absolute");
          ("left", px realPosition.x);
          ("top", px realPosition.y);
          ("color", "white");
          ("display", "flex");
          ("align-items", "center");
          ("justify-content", "center");
        ];
    ]
    [ text "Drag Me!" ]

let main = standardProgram { init; update; view; subscriptions }
