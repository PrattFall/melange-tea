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

  let children n = n.vdoms

  let replace elem oldChild newChild =
    ignore (Web.Node.insert_before elem newChild oldChild);
    ignore (Web.Node.remove_child elem oldChild)

  let flip elem first second =
    ignore (Web.Node.remove_child elem second);
    ignore (Web.Node.insert_before elem second first)

  let add_to_document elem =
    Web.Document.create_element ~namespace:elem.namespace elem.tag_name
end

module LazyGen = struct
  type 'container t = {
    key : string;
    gen : unit -> 'container;
    cache : 'container ref;
  }

  let make key gen cache = { key; gen; cache }
  let equals g1 g2 = g1.key = g2.key
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

  let rec create_dom_node callbacks newNode =
    let open DomNode in
    let new_child = DomNode.add_to_document newNode in

    ignore
      (Property.apply callbacks new_child
         (Property.map_empty newNode.props)
         newNode.props);

    patch callbacks new_child [||] 0 [] newNode.vdoms;

    new_child

  and create_element callbacks = function
    | CommentNode s -> Web.Document.create_comment s
    | Text text -> Web.Document.create_text_node text
    | Node new_node -> create_dom_node callbacks new_node
    | LazyGen new_lazy ->
        let vdom = new_lazy.gen () in
        new_lazy.cache := vdom;
        create_element callbacks vdom
    | Tagger (tagger, vdom) -> create_element (tagger callbacks) vdom

  and mutate callbacks parent children child_index old_node new_node =
    match (old_node, new_node) with
    | Node old, Node nw ->
        (* Different node entirely. Replace it *)
        if old.unique <> nw.unique || old.tag_name <> nw.tag_name then
          let oldChild = children.(child_index) in
          let newChild = create_element callbacks new_node in
          DomNode.replace parent oldChild newChild
        else
          let child = children.(child_index) in
          let grandChildren = Web.Node.child_nodes child in
          Property.apply callbacks child old.props nw.props;
          patch callbacks child grandChildren 0 old.vdoms nw.vdoms
    | _ -> failwith "Non-node passed to mutate"

  (** Rectify the differences between two VDom Node lists *)
  and patch callbacks parent children child_index old_vnodes new_vnodes =
    match (old_vnodes, new_vnodes) with
    | [], [] -> ()
    | Tagger (_, old_vdom) :: old_rest, _ ->
        patch callbacks parent children child_index (old_vdom :: old_rest)
          new_vnodes
    | old_node :: old_rest, Tagger (new_tagger, new_vdom) :: new_rest ->
        patch (new_tagger callbacks) parent children child_index [ old_node ]
          [ new_vdom ];
        patch callbacks parent children (child_index + 1) old_rest new_rest
    | [], new_node :: new_rest ->
        let new_child = create_element callbacks new_node in
        ignore (Web.Node.append_child parent new_child);
        patch callbacks parent children (child_index + 1) [] new_rest
    | _ :: oldRest, [] ->
        let child = children.(child_index) in
        ignore (Web.Node.remove_child parent child);
        patch callbacks parent children child_index oldRest []
    | CommentNode old_comment :: old_rest, CommentNode new_comment :: new_rest
      when old_comment = new_comment ->
        patch callbacks parent children (child_index + 1) old_rest new_rest
    | Text old_text :: old_rest, Text new_text :: new_rest ->
        (if old_text <> new_text then
           let child = children.(child_index) in
           Web.Node.set_value child new_text);
        patch callbacks parent children (child_index + 1) old_rest new_rest
    | LazyGen old_lazy :: old_rest, LazyGen new_lazy :: new_rest -> (
        if LazyGen.equals old_lazy new_lazy then (
          new_lazy.cache := !(old_lazy.cache);
          patch callbacks parent children (child_index + 1) old_rest new_rest)
        else
          match (old_rest, new_rest) with
          | LazyGen older_lazy :: older_rest, LazyGen newer_lazy :: newer_rest
            when LazyGen.equals older_lazy new_lazy
                 && LazyGen.equals old_lazy newer_lazy ->
              let first_child = children.(child_index) in
              let second_child = children.(child_index + 1) in
              DomNode.flip parent first_child second_child;
              patch callbacks parent children (child_index + 2) older_rest
                newer_rest
          | LazyGen older_lazy :: older_rest, _
            when LazyGen.equals older_lazy new_lazy ->
              let old_child = children.(child_index) in
              ignore (Web.Node.remove_child parent old_child);
              let old_vdom = !(older_lazy.cache) in
              new_lazy.cache := old_vdom;
              patch callbacks parent children (child_index + 1) older_rest
                new_rest
          | _, LazyGen newer_lazy :: _ when LazyGen.equals newer_lazy old_lazy
            ->
              let old_child = children.(child_index) in
              let new_vdom = new_lazy.gen () in
              new_lazy.cache := new_vdom;
              let new_child = create_element callbacks new_vdom in
              ignore (Web.Node.insert_before parent new_child old_child);
              patch callbacks parent children (child_index + 1) old_vnodes
                new_rest
          | _ ->
              let old_vdom = !(old_lazy.cache) in
              let new_vdom = new_lazy.gen () in
              new_lazy.cache := new_vdom;
              patch callbacks parent children child_index (old_vdom :: old_rest)
                (new_vdom :: new_rest))
    | Node old_node :: old_rest, Node new_node :: new_rest -> (
        if old_node.key = new_node.key && old_node.key <> "" then
          (* Found the same node so move on *)
          patch callbacks parent children (child_index + 1) old_rest new_rest
        else if old_node.key = "" || new_node.key = "" then (
          (* one of the nodes doesn't have a key so mutate the node? *)
          mutate callbacks parent children child_index (Node old_node)
            (Node new_node);
          patch callbacks parent children (child_index + 1) old_rest new_rest)
        else
          (* oldNode has a key that doesn't match newNode so drill into the rest of the node list *)
          match (old_rest, new_rest) with
          | Node older_node :: older_rest, Node newer_node :: newer_rest
            when DomNode.equals older_node new_node
                 && DomNode.equals old_node newer_node ->
              (* The nodes swapped so flip the actual nodes in the DOM *)
              let first_child = children.(child_index) in
              let second_child = children.(child_index + 1) in
              DomNode.flip parent first_child second_child;
              patch callbacks parent children (child_index + 2) older_rest
                newer_rest
          | Node older_node :: older_rest, _
            when DomNode.equals older_node new_node ->
              (* Old node must have been deleted, so delete it from the DOM *)
              let old_child = children.(child_index) in
              ignore (Web.Node.remove_child parent old_child);
              patch callbacks parent children (child_index + 1) older_rest
                new_rest
          | _, Node newer_node :: _ when DomNode.equals old_node newer_node ->
              (* New node must have been added, so create it in the DOM *)
              let old_child = children.(child_index) in
              let new_child = create_element callbacks (Node new_node) in
              let _attachedChild =
                Web.Node.insert_before parent new_child old_child
              in
              patch callbacks parent children (child_index + 1) old_vnodes
                new_rest
          | _ ->
              (* Something else happened? Better mutate *)
              mutate callbacks parent children child_index (Node old_node)
                (Node new_node);
              patch callbacks parent children (child_index + 1) old_rest
                new_rest)
    | _ :: old_rest, new_node :: new_rest ->
        let old_child = children.(child_index) in
        let new_child = create_element callbacks new_node in
        DomNode.replace parent old_child new_child;
        patch callbacks parent children (child_index + 1) old_rest new_rest

  let patch_nodes_into_element callbacks parent old_vnodes new_vnodes =
    let children = Web.Node.child_nodes parent in
    patch callbacks parent children 0 old_vnodes new_vnodes;
    new_vnodes

  let patch_node_into_element callbacks parent old_vnode new_vnode =
    patch_nodes_into_element callbacks parent [ old_vnode ] [ new_vnode ]
end

let map : ('a -> 'b) -> 'a Node.t -> 'b Node.t =
 fun func vdom ->
  let tagger = ApplicationCallbacks.wrap_callbacks func in
  Tagger (Obj.magic tagger, Obj.magic vdom)
