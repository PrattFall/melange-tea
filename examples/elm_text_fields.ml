open Tea.Html
open Tea.Html.Attributes
open Tea.Html.Events

type model = { content : string }
type msg = Change of string

let update _ = function Change s -> { content = s }

let init = {
    content = ""
}

(* https://discuss.ocaml.org/t/should-we-have-a-string-rev-in-stdlib/9187/5 *)
let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let view model =
  div []
    [
      input'
        [
          placeholder "Text to reverse";
          defaultValue model.content;
          onInput (fun x -> Change x);
        ]
        [];
      div [] [ text (rev model.content) ];
    ]
