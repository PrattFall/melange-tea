open Tea.Html

let view () =
  div []
    [
      h1 [] [ text "My Grocery List" ];
      ul []
        [
          li [] [ text "Black Beans" ];
          li [] [ text "Limes" ];
          li [] [ text "Greek Yogurt" ];
          li [] [ text "Cilantro" ];
          li [] [ text "Honey" ];
          li [] [ text "Sweet Potatoes" ];
          li [] [ text "Cumin" ];
          li [] [ text "Chili Powder" ];
          li [] [ text "Quinoa" ];
        ];
    ]
