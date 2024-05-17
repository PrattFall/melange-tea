open Tea.App
open Tea.Html
open Tea.Html.Attributes

type model = string * string

let init = ("key", "value")

type message = ChangeKeyText of string | ChangeValueText of string

let update model = function
  | ChangeKeyText k ->
      let _, value = model in
      (k, value)
  | ChangeValueText v ->
      let key, _ = model in
      (key, v)

let view model =
  let data_key, data_value = model in
  div []
    [
      node "style" []
        [
          text
            (".data-display:before { content: 'data-" ^ data_key
           ^ ": ' attr(data-" ^ data_key ^ ")");
        ];
      p []
        [
          text
            "The following textboxes change the data-attribute of the div \
             underneath. The div's content is then being set via CSS to show \
             the value of that data-attribute.";
        ];
      input'
        [
          type' "text";
          defaultValue data_key;
          Events.onChange (fun x -> ChangeKeyText x);
        ]
        [];
      input'
        [
          type' "text";
          defaultValue data_value;
          Events.onChange (fun x -> ChangeValueText x);
        ]
        [];
      div [ data data_key data_value; class' "data-display" ] [];
    ]

let main = Tea.App.beginnerProgram { model = init; update; view }
