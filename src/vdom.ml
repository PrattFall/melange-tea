module SystemMessage = struct
  type 'msg t = Render | AddRenderMsg of 'msg | RemoveRenderMsg of 'msg

  let wrap_callbacks_on func = function
    | Render -> Render
    | AddRenderMsg msg -> AddRenderMsg (func msg)
    | RemoveRenderMsg msg -> RemoveRenderMsg (func msg)
end

module ApplicationCallbacks = struct
  type 'msg t = { enqueue : 'msg -> unit; on : 'msg SystemMessage.t -> unit }

  let enqueue callbacksRef fn = !callbacksRef.enqueue fn
  let on systemMessage callbacksRef = !callbacksRef.on systemMessage

  let wrap_callbacks func callbacks =
    Obj.magic ref
      {
        enqueue = (fun msg -> enqueue callbacks (func msg));
        on =
          (fun smsg ->
            smsg |> SystemMessage.wrap_callbacks_on func |. on callbacks);
      }
end

module EventHandler = struct
  type 'msg t =
    | EventHandlerCallback of string * (Dom.event -> 'msg option)
    | EventHandlerMsg of 'msg

  type 'msg cache = {
    handler : Dom.event -> unit;
    cb : (Dom.event -> 'msg option) ref;
  }

  let emptyEventCB _ev : 'e Web.Event.callback option = None

  let eventHandler callbacks cb ev =
    Option.iter (ApplicationCallbacks.enqueue callbacks) (!cb ev)

  let callback key fn = EventHandlerCallback (key, fn)

  let get_callback = function
    | EventHandlerCallback (_, cb) -> cb
    | EventHandlerMsg (msg : 'msg) -> fun _ev -> Some msg

  let equals left right =
    match (left, right) with
    | EventHandlerCallback (lcb, _), EventHandlerCallback (rcb, _) -> lcb = rcb
    | EventHandlerMsg lmsg, EventHandlerMsg rmsg -> lmsg = rmsg
    | _ -> false

  let register callbacks elem event_name handlerType =
    let cb = ref (get_callback handlerType) in
    let handler = eventHandler callbacks cb in
    Web.Node.add_event_listener elem event_name handler;
    Some { handler; cb }

  let unregister (elem : 'a Dom.node_like) (event_name : string) = function
    | None -> None
    | Some cache ->
        Web.Node.remove_event_listener elem event_name cache.handler;
        None
end

module Attribute = struct
  type t = { namespace : string; key : string; value : string }

  let make ?(namespace = "") key value = { namespace; key; value }
  let equals a1 a2 = a1.namespace = a2.namespace && a1.key = a2.key
  let to_string a = String.concat "" [ " "; a.key; "=\""; a.value; "\"" ]

  let apply_to_element elem a =
    Web.Node.set_attribute ~namespace:a.namespace elem a.key a.value

  let remove_from_element elem a =
    Web.Node.remove_attribute ~namespace:a.namespace elem a.key

  let mutate_on_element elem a =
    Web.Node.set_attribute ~namespace:a.namespace elem a.key a.value
end

module Event = struct
  type 'msg t = {
    name : string;
    handler : 'msg EventHandler.t;
    cache : 'msg EventHandler.cache option ref;
  }

  let equals e1 e2 = e1.name = e2.name
  let make name handler cache = { name; handler; cache }

  let apply_to_element callbacks elem e =
    e.cache := EventHandler.register callbacks elem e.name e.handler

  let remove_from_element elem e =
    e.cache := EventHandler.unregister elem e.name !(e.cache)

  let mutate_handler callbacks elem oldE newE =
    let open EventHandler in
    match !(oldE.cache) with
    | None -> newE.cache := register callbacks elem newE.name newE.handler
    | Some oldcache ->
        if oldE.name = newE.name then (
          newE.cache := !(oldE.cache);
          if equals oldE.handler newE.handler then ()
          else oldcache.cb := get_callback newE.handler)
        else (
          oldE.cache := unregister elem oldE.name !(oldE.cache);
          newE.cache := register callbacks elem newE.name newE.handler)
end

module Style = struct
  type t = (string * string) list

  let to_string s =
    String.concat ""
      [
        " style=\"";
        String.concat ";"
          (List.map (fun (k, v) -> String.concat "" [ k; ":"; v; ";" ]) s);
        "\"";
      ]

  let apply_to_element elem s =
    List.iter (fun (k, v) -> Web.Node.set_style_property elem k (Some v)) s

  let remove_from_element elem s =
    List.iter (fun (k, _) -> Web.Node.set_style_property elem k None) s

  let mutate_on_element elem oldStyles newStyles =
    (* Works for now  *)
    List.iter
      (fun (key, _) -> Web.Node.set_style_property elem key None)
      oldStyles;
    List.iter
      (fun (key, value) -> Web.Node.set_style_property elem key (Some value))
      newStyles
end

module DataAttribute = struct
  type t = string * string

  let to_string (k, v) = String.concat "" [ " data-"; k; "=\""; v; "\"" ]
  let equals (k1, _) (k2, _) = k1 = k2

  let apply_to_element elem (k, v) =
    let dataset = Web.Node.dataset elem in
    Js.Dict.set dataset k v

  let remove_from_element elem (k, _) = Web.Node.remove_from_dataset elem k

  let mutate_on_element elem (k, v) =
    let dataset = Web.Node.dataset elem in
    Js.Dict.set dataset k v
end

module Property = struct
  type 'msg t =
    | NoProp
    | RawProp of string * string
    | Attribute of Attribute.t
    | Data of DataAttribute.t
    | Event of 'msg Event.t
    | Style of Style.t

  let empty = NoProp

  let attribute ?(namespace = "") key value =
    Attribute (Attribute.make ~namespace key value)

  let prop key value = RawProp (key, value)

  let onCB name key cb =
    Event (Event.make name (EventHandlerCallback (key, cb)) (ref None))

  let onMsg name msg = Event (Event.make name (EventHandlerMsg msg) (ref None))
  let data key value = Data (key, value)
  let style key value = Style [ (key, value) ]
  let styles s = Style s

  (** Convert a list of Properties to NoProps *)
  let map_empty props = List.map (fun _ -> empty) props

  (** Compare two Properties based on their namespaces and keys *)
  let equals x y =
    match (x, y) with
    | NoProp, NoProp -> true
    | RawProp (k1, _), RawProp (k2, _) -> k1 = k2
    | Attribute a1, Attribute a2 -> Attribute.equals a1 a2
    | Data d1, Data d2 -> DataAttribute.equals d1 d2
    | Event e1, Event e2 -> Event.equals e1 e2
    | Style _, Style _ -> true
    | _ -> false

  (** Convert a Property to its HTML string representation *)
  let to_string = function
    | RawProp (k, v) -> String.concat "" [ " "; k; "=\""; v; "\"" ]
    | Attribute a -> Attribute.to_string a
    | Data d -> DataAttribute.to_string d
    | Style s -> Style.to_string s
    | _ -> ""

  let apply_to_element callbacks elem = function
    | NoProp -> ()
    | RawProp (k, v) -> Web.Node.set_prop elem k v
    | Attribute a -> Attribute.apply_to_element elem a
    | Data d -> DataAttribute.apply_to_element elem d
    | Event e -> Event.apply_to_element callbacks elem e
    | Style s -> Style.apply_to_element elem s

  let remove_from_element elem = function
    | NoProp -> ()
    | RawProp (k, _) -> Web.Node.set_prop elem k Js.Undefined.empty
    | Attribute a -> Attribute.remove_from_element elem a
    | Data d -> DataAttribute.remove_from_element elem d
    | Event e -> Event.remove_from_element elem e
    | Style s -> Style.remove_from_element elem s

  (** Replace a Property on an Element with another Property *)
  let replace_on_element callbacks elem oldProp newProp =
    remove_from_element elem oldProp;
    apply_to_element callbacks elem newProp

  (** Modify a Property on an Element using values from another Property.
      (At the moment replaces everything) *)
  let mutate_on_element callbacks elem oldProp newProp =
    match (oldProp, newProp) with
    | Style oldStyles, Style newStyles ->
        Style.mutate_on_element elem oldStyles newStyles
    | Event oldE, Event newE -> Event.mutate_handler callbacks elem oldE newE
    (* These just get replaced *)
    | _, RawProp (k, v) -> Web.Node.set_prop elem k v
    | _, Attribute a -> Attribute.mutate_on_element elem a
    | _, Data d -> DataAttribute.mutate_on_element elem d
    | _ -> ()

  let apply callbacks elem old_properties new_properties =
    let updated_new, created =
      List.partition
        (fun a -> List.exists (equals a) old_properties)
        new_properties
    in

    let deletable =
      List.filter
        (fun a -> not (List.exists (equals a) new_properties))
        old_properties
    in

    let updated =
      updated_new
      |> List.map (fun newProp ->
             List.find_opt (equals newProp) old_properties
             |> Option.map (fun oldProp -> (oldProp, newProp)))
      |> List.map Option.to_list |> List.flatten
    in

    List.iter (remove_from_element elem) deletable;
    List.iter
      (fun (old_prop, new_prop) ->
        mutate_on_element callbacks elem old_prop new_prop)
      updated;
    List.iter (apply_to_element callbacks elem) created
end

module DomNode = struct
  type ('msg, 'container) t = {
    namespace : string;
    tag_name : string;
    key : string;
    unique : string;
    props : 'msg Property.t list;
    vdoms : 'container list;
  }

  let equals n1 n2 =
    n1.namespace = n2.namespace && n1.tag_name = n2.tag_name && n1.key = n2.key

  let make ?(namespace = "") ?(key = "") ?(unique = "") tag_name props vdoms =
    { namespace; tag_name; key; unique; props; vdoms }

  let add_to_document elem =
    Web.Document.create_element ~namespace:elem.namespace elem.tag_name

  let from_node callbacks new_node =
    let new_child = add_to_document new_node in

    ignore
      (Property.apply callbacks new_child
         (Property.map_empty new_node.props)
         new_node.props);

    new_child

  let children n = n.vdoms
  let should_flip old nw older newer = equals older nw && equals old newer

  let replace elem oldChild newChild =
    ignore (Web.Node.insert_before elem newChild oldChild);
    ignore (Web.Node.remove_child elem oldChild)

  let flip elem first second =
    ignore (Web.Node.remove_child elem second);
    ignore (Web.Node.insert_before elem second first)
end

module LazyGen = struct
  type 'container t = {
    key : string;
    gen : unit -> 'container;
    cache : 'container ref;
  }

  let make key gen cache = { key; gen; cache }
  let equals g1 g2 = g1.key = g2.key
  let should_flip old nw older newer = equals older nw && equals old newer

  let remove_child parent child old_lazy new_lazy =
    ignore (Web.Node.remove_child parent child);
    let old_vdom = !(old_lazy.cache) in
    new_lazy.cache := old_vdom

  let create_child callbacks creater parent old_child new_lazy =
    let new_vdom = new_lazy.gen () in
    new_lazy.cache := new_vdom;
    let new_child = creater callbacks new_vdom in
    ignore (Web.Node.insert_before parent new_child old_child)

  let mutate old_lazy new_lazy =
    let old_vdom = !(old_lazy.cache) in
    let new_vdom = new_lazy.gen () in
    new_lazy.cache := new_vdom;
    (old_vdom, new_vdom)
end

module Node = struct
  type 'msg t =
    | CommentNode of string
    | Text of string
    | Node of ('msg, 'msg t) DomNode.t
    | LazyGen of 'msg t LazyGen.t
    | Tagger of
        ('msg ApplicationCallbacks.t ref -> 'msg ApplicationCallbacks.t ref)
        * 'msg t

  let empty = CommentNode ""
  let comment s = CommentNode s
  let text s = Text s

  let fullnode namespace tag_name key unique props vdoms =
    Node (DomNode.make ~namespace ~key ~unique tag_name props vdoms)

  let node ?(namespace = "") ?(key = "") ?(unique = "") tagName props vdoms =
    fullnode namespace tagName key unique props vdoms

  let lazyGen key fn = LazyGen (LazyGen.make key fn (ref empty))

  let rec to_string = function
    | CommentNode s -> "<!-- " ^ s ^ " -->"
    | Text s -> s
    | Node n ->
        String.concat ""
          [
            "<";
            n.namespace;
            (if n.namespace = "" then "" else ":");
            n.tag_name;
            String.concat "" (List.map Property.to_string n.props);
            ">";
            String.concat "" (List.map to_string n.vdoms);
            "</";
            n.tag_name;
            ">";
          ]
    | LazyGen lzy -> to_string (lzy.gen ())
    | Tagger (_, vdom) -> to_string vdom

  let rec patch callbacks parent children old_vnodes new_vnodes =
    let tail_or_all = function [] -> [] | _ :: ls -> ls in

    let rec create_element callbacks = function
      | CommentNode s -> Web.Document.create_comment s
      | Text text -> Web.Document.create_text_node text
      | Node new_node ->
          let new_child = DomNode.from_node callbacks new_node in
          patch callbacks new_child [] [] new_node.vdoms;
          new_child
      | LazyGen new_lazy ->
          let vdom = new_lazy.gen () in
          new_lazy.cache := vdom;
          create_element callbacks vdom
      | Tagger (tagger, vdom) -> create_element (tagger callbacks) vdom
    in

    let mutate callbacks parent children old_node new_node =
      match (old_node, new_node) with
      | Node old, Node nw -> (
          match children with
          | child :: _ ->
              if old.unique <> nw.unique || old.tag_name <> nw.tag_name then
                DomNode.replace parent child (create_element callbacks new_node)
              else
                let grandchildren =
                  Array.to_list (Web.Node.child_nodes child)
                in
                Property.apply callbacks child old.props nw.props;
                patch callbacks child grandchildren old.vdoms nw.vdoms
          | _ -> failwith "Not enough children passed to mutate")
      | _ -> failwith "Non-node passed to mutate"
    in

    let continue children old_rest new_rest =
      patch callbacks parent children old_rest new_rest
    in

    let next children old_rest new_rest =
      continue (tail_or_all children) old_rest new_rest
    in

    let set_text_value children old_text new_text =
      if old_text <> new_text then
        match children with
        | child :: _ -> Web.Node.set_value child new_text
        | _ -> failwith "Not enough children for set_value"
    in

    match (old_vnodes, new_vnodes) with
    | [], [] -> ()
    | old_node :: old_rest, new_node :: new_rest -> (
        match (old_node, new_node) with
        | Tagger (_, old_vdom), _ ->
            continue children (old_vdom :: old_rest) new_vnodes
        | _, Tagger (new_tagger, new_vdom) ->
            patch (new_tagger callbacks) parent children [ old_node ]
              [ new_vdom ];
            next children old_rest new_rest
        | CommentNode old_comment, CommentNode new_comment
          when old_comment = new_comment ->
            next children old_rest new_rest
        | Text old_text, Text new_text ->
            set_text_value children old_text new_text;
            next children old_rest new_rest
        | LazyGen old_lazy, LazyGen new_lazy
          when LazyGen.equals old_lazy new_lazy ->
            new_lazy.cache := !(old_lazy.cache);
            next children old_rest new_rest
        | LazyGen old_lazy, LazyGen new_lazy -> (
            match (old_rest, new_rest) with
            | older_node :: older_rest, newer_node :: newer_rest -> (
                match (older_node, newer_node) with
                | LazyGen older_lazy, LazyGen newer_lazy
                  when LazyGen.should_flip old_lazy new_lazy older_lazy
                         newer_lazy -> (
                    match children with
                    | first_child :: second_child :: children ->
                        DomNode.flip parent first_child second_child;
                        continue children older_rest newer_rest
                    | _ ->
                        failwith
                          "Not enough child elements left for LazyGen flip")
                | LazyGen older_lazy, _ when LazyGen.equals older_lazy new_lazy
                  -> (
                    match children with
                    | old_child :: children ->
                        LazyGen.remove_child parent old_child older_lazy
                          new_lazy;
                        continue children older_rest new_rest
                    | _ ->
                        failwith
                          "Not enough child elements left for LazyGen remove")
                | _, LazyGen newer_lazy when LazyGen.equals newer_lazy old_lazy
                  -> (
                    match children with
                    | old_child :: children ->
                        LazyGen.create_child callbacks create_element parent
                          old_child new_lazy;
                        continue children old_vnodes new_rest
                    | _ ->
                        failwith
                          "Not enough child elements left for LazyGen create")
                | _ ->
                    let old_vdom, new_vdom = LazyGen.mutate old_lazy new_lazy in
                    continue children (old_vdom :: old_rest)
                      (new_vdom :: new_rest))
            | _ -> ())
        | Node old_n, Node new_n when old_n.key = "" || new_n.key = "" ->
            mutate callbacks parent children old_node new_node;
            next children old_rest new_rest
        | Node old_n, Node new_n when old_n.key = new_n.key ->
            next children old_rest new_rest
        | Node old_n, Node new_n -> (
            match (old_rest, new_rest) with
            | older_node :: older_rest, newer_node :: newer_rest -> (
                match (older_node, newer_node) with
                | Node older_n, Node newer_n
                  when DomNode.should_flip old_n new_n older_n newer_n -> (
                    match children with
                    | first_child :: second_child :: children ->
                        DomNode.flip parent first_child second_child;
                        continue children older_rest newer_rest
                    | _ -> failwith "Not enough children left for DomNode flip")
                | Node older_n, _ when DomNode.equals older_n new_n -> (
                    match children with
                    | old_child :: children ->
                        ignore (Web.Node.remove_child parent old_child);
                        continue children old_vnodes new_rest
                    | _ ->
                        failwith "Not enough children left for DomNode remove")
                | _, Node newer_n when DomNode.equals old_n newer_n -> (
                    (* New node must have been added, so create it in the DOM *)
                    match children with
                    | old_child :: children ->
                        let new_child = create_element callbacks new_node in
                        ignore
                          (Web.Node.insert_before parent new_child old_child);
                        continue children old_vnodes new_rest
                    | _ ->
                        failwith "Not enough children left for DomNode insert")
                | _ ->
                    mutate callbacks parent children old_node new_node;
                    next children old_rest new_rest)
            | _ ->
                mutate callbacks parent children old_node new_node;
                next children old_rest new_rest)
        | _, new_node -> (
            match children with
            | old_child :: children ->
                let new_child = create_element callbacks new_node in
                DomNode.replace parent old_child new_child;
                continue children old_rest new_rest
            | _ -> failwith "Not enough children for DomNode.replace"))
    | [], new_node :: new_rest ->
        let new_child = create_element callbacks new_node in
        ignore (Web.Node.append_child parent new_child);
        next children [] new_rest
    | _ :: oldRest, [] -> (
        match children with
        | child :: children ->
            ignore (Web.Node.remove_child parent child);
            continue children oldRest []
        | _ -> failwith "Not enough children to remove")

  let patch_nodes_into_element callbacks parent old_vnodes new_vnodes =
    let children = Array.to_list (Web.Node.child_nodes parent) in
    patch callbacks parent children old_vnodes new_vnodes;
    new_vnodes

  let patch_node_into_element callbacks parent old_vnode new_vnode =
    patch_nodes_into_element callbacks parent [ old_vnode ] [ new_vnode ]
end

let map : ('a -> 'b) -> 'a Node.t -> 'b Node.t =
 fun func vdom ->
  let tagger = ApplicationCallbacks.wrap_callbacks func in
  Tagger (Obj.magic tagger, Obj.magic vdom)
