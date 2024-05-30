open Tea.Svg
open Tea.Svg.Attributes

let view () =
  svg
    [ viewBox "0 0 400 400"; width "400"; height "400" ]
    [
      circle
        [
          cx "50"; cy "50"; r "40"; fill "red"; stroke "black"; strokeWidth "3";
        ]
        [];
      rect
        [
          x "100";
          y "10";
          width "40";
          height "40";
          fill "green";
          stroke "black";
          strokeWidth "2";
        ]
        [];
      line
        [
          x1 "20";
          y1 "200";
          x2 "200";
          y2 "20";
          stroke "blue";
          strokeWidth "10";
          strokeLinecap "round";
        ]
        [];
      polyline
        [
          points "200,40 240,40 240,80 280,80 280,120 320,120 320,160";
          fill "none";
          stroke "red";
          strokeWidth "4";
          strokeDasharray "20,2";
        ]
        [];
      text'
        [
          x "130";
          y "130";
          fill "white";
          textAnchor "middle";
          dominantBaseline "central";
          transform "rotate(-45 130,130)";
        ]
        [ text "Welcome to the Shapes Club" ];
    ]
