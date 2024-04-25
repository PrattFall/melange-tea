let focus id =
  Tea_cmd.call (fun _enqueue ->
      let ecb _ =
        match Web.Document.get_element_by_id' id with
        | None -> Js.log ("Attempted to focus a non-existant element of: ", id)
        | Some elem -> Web_node.focus elem
      in
      (* One to get out of the current render frame*)
      let cb _ = ignore (Web.Window.request_animation_frame ecb) in
      (* And another to properly focus *)
      ignore (Web.Window.request_animation_frame cb);
      ())
