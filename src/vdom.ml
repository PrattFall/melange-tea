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

module Property = struct
  type 'msg t =
    | NoProp
    | RawProp of string * string
    | Attribute of Attribute.t
    | Data of string * string
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
    | Data (k1, _), Data (k2, _) -> k1 = k2
    | Event e1, Event e2 -> Event.equals e1 e2
    | Style _, Style _ -> true
    | _ -> false

  (** Convert a Property to its HTML string representation *)
  let to_string = function
    | RawProp (k, v) -> String.concat "" [ " "; k; "=\""; v; "\"" ]
    | Attribute a -> Attribute.to_string a
    | Data (k, v) -> String.concat "" [ " data-"; k; "=\""; v; "\"" ]
    | Style s -> Style.to_string s
    | _ -> ""

  let apply_to_element callbacks elem = function
    | NoProp -> ()
    | RawProp (k, v) -> Web.Node.set_prop elem k v
    | Attribute a -> Attribute.apply_to_element elem a
    | Data (k, v) ->
        Js.log ("TODO:  Add Data Unhandled", k, v);
        failwith "TODO:  Add Data Unhandled"
    | Event e -> Event.apply_to_element callbacks elem e
    | Style s -> Style.apply_to_element elem s

  let remove_from_element elem = function
    | NoProp -> ()
    | RawProp (k, _) -> Web.Node.set_prop elem k Js.Undefined.empty
    | Attribute a -> Attribute.remove_from_element elem a
    | Data (k, v) ->
        Js.log ("TODO:  Remove Data Unhandled", k, v);
        failwith "TODO:  Remove Data Unhandled"
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
    | _, Data (k, v) ->
        Js.log ("TODO:  Mutate Data Unhandled", k, v);
        failwith "TODO:  Mutate Data Unhandled"
    | _ -> ()

  let apply callbacks elem oldProperties newProperties =
    let updatedNew, created =
      List.partition
        (fun a -> List.exists (equals a) oldProperties)
        newProperties
    in

    let deletable =
      List.filter
        (fun a -> not (List.exists (equals a) newProperties))
        oldProperties
    in

    let updated =
      updatedNew
      |> List.map (fun newProp ->
             List.find_opt (equals newProp) oldProperties
             |> Option.map (fun oldProp -> (oldProp, newProp)))
      |> List.map Option.to_list |> List.flatten
    in

    List.iter (remove_from_element elem) deletable;
    List.iter
      (fun (oldProp, newProp) ->
        mutate_on_element callbacks elem oldProp newProp)
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
    let newChild =
      Web.Document.create_element ~namespace:newNode.namespace newNode.tag_name
    in

    ignore
      (Property.apply callbacks newChild
         (Property.map_empty newNode.props)
         newNode.props);

    let grandChildren = Web.Node.child_nodes newChild in

    patch_nodes callbacks newChild grandChildren 0 [] newNode.vdoms;

    newChild

  and replace callbacks elem elems idx = function
    | Node newNode ->
        let oldChild = elems.(idx) in
        let newChild = create_dom_node callbacks newNode in
        DomNode.replace elem oldChild newChild
    | _ -> failwith "Node replacement must only be passed a Node"

  and create_element callbacks = function
    | CommentNode s -> Web.Document.create_comment s
    | Text text -> Web.Document.create_text_node text
    | Node newNode -> create_dom_node callbacks newNode
    | LazyGen newLazy ->
        let vdom = newLazy.gen () in
        newLazy.cache := vdom;
        create_element callbacks vdom
    | Tagger (tagger, vdom) -> create_element (tagger callbacks) vdom

  and mutate callbacks elem elems idx oldNode newNode =
    match (oldNode, newNode) with
    | Node old, Node nw ->
        if old.unique <> nw.unique || old.tag_name <> nw.tag_name then
          replace callbacks elem elems idx newNode
        else
          let child = elems.(idx) in
          let grandChildren = Web.Node.child_nodes child in
          Property.apply callbacks child old.props nw.props;
          patch_nodes callbacks child grandChildren 0 old.vdoms nw.vdoms
    | _ -> failwith "Non-node passed to mutate"

  (** Rectify the differences between two VDom Node lists *)
  and patch_nodes callbacks elem elems idx oldVNodes newVNodes =
    match (oldVNodes, newVNodes) with
    | [], [] -> ()
    | Tagger (_, oldVdom) :: oldRest, _ ->
        patch_nodes callbacks elem elems idx (oldVdom :: oldRest) newVNodes
    | oldNode :: oldRest, Tagger (newTagger, newVdom) :: newRest ->
        patch_nodes (newTagger callbacks) elem elems idx [ oldNode ] [ newVdom ];
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest
    | [], newNode :: newRest ->
        let newChild = create_element callbacks newNode in
        ignore (Web.Node.append_child elem newChild);
        patch_nodes callbacks elem elems (idx + 1) [] newRest
    | _ :: oldRest, [] ->
        let child = elems.(idx) in
        ignore (Web.Node.remove_child elem child);
        patch_nodes callbacks elem elems idx oldRest []
    | CommentNode oldS :: oldRest, CommentNode newS :: newRest when oldS = newS
      ->
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest
    | Text oldText :: oldRest, Text newText :: newRest ->
        (if oldText <> newText then
           let child = elems.(idx) in
           Web.Node.set_value child newText);
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest
    | LazyGen oldLazy :: oldRest, LazyGen newLazy :: newRest -> (
        if LazyGen.equals oldLazy newLazy then (
          newLazy.cache := !(oldLazy.cache);
          patch_nodes callbacks elem elems (idx + 1) oldRest newRest)
        else
          match (oldRest, newRest) with
          | LazyGen olderLazy :: olderRest, LazyGen newerLazy :: newerRest
            when LazyGen.equals olderLazy newLazy
                 && LazyGen.equals oldLazy newerLazy ->
              let firstChild = elems.(idx) in
              let secondChild = elems.(idx + 1) in
              DomNode.flip elem firstChild secondChild;
              patch_nodes callbacks elem elems (idx + 2) olderRest newerRest
          | LazyGen olderLazy :: olderRest, _
            when LazyGen.equals olderLazy newLazy ->
              let oldChild = elems.(idx) in
              ignore (Web.Node.remove_child elem oldChild);
              let oldVdom = !(olderLazy.cache) in
              newLazy.cache := oldVdom;
              patch_nodes callbacks elem elems (idx + 1) olderRest newRest
          | _, LazyGen newerLazy :: _ when LazyGen.equals newerLazy oldLazy ->
              let oldChild = elems.(idx) in
              let newVdom = newLazy.gen () in
              newLazy.cache := newVdom;
              let newChild = create_element callbacks newVdom in
              ignore (Web.Node.insert_before elem newChild oldChild);
              patch_nodes callbacks elem elems (idx + 1) oldVNodes newRest
          | _ ->
              let oldVdom = !(oldLazy.cache) in
              let newVdom = newLazy.gen () in
              newLazy.cache := newVdom;
              patch_nodes callbacks elem elems idx (oldVdom :: oldRest)
                (newVdom :: newRest))
    | Node oldNode :: oldRest, Node newNode :: newRest -> (
        if oldNode.key = newNode.key && oldNode.key <> "" then
          patch_nodes callbacks elem elems (idx + 1) oldRest newRest
        else if oldNode.key = "" || newNode.key = "" then (
          mutate callbacks elem elems idx (Node oldNode) (Node newNode);
          patch_nodes callbacks elem elems (idx + 1) oldRest newRest)
        else
          match (oldRest, newRest) with
          | Node olderNode :: olderRest, Node newerNode :: newerRest
            when DomNode.equals olderNode newNode
                 && DomNode.equals oldNode newerNode ->
              let firstChild = elems.(idx) in
              let secondChild = elems.(idx + 1) in
              DomNode.flip elem firstChild secondChild;
              patch_nodes callbacks elem elems (idx + 2) olderRest newerRest
          | Node olderNode :: olderRest, _ when DomNode.equals olderNode newNode
            ->
              let oldChild = elems.(idx) in
              ignore (Web.Node.remove_child elem oldChild);
              patch_nodes callbacks elem elems (idx + 1) olderRest newRest
          | _, Node newerNode :: _ when DomNode.equals oldNode newerNode ->
              let oldChild = elems.(idx) in
              let newChild = create_element callbacks (Node newNode) in
              let _attachedChild =
                Web.Node.insert_before elem newChild oldChild
              in
              patch_nodes callbacks elem elems (idx + 1) oldVNodes newRest
          | _ ->
              mutate callbacks elem elems idx (Node oldNode) (Node newNode);
              patch_nodes callbacks elem elems (idx + 1) oldRest newRest)
    | _ :: oldRest, newNode :: newRest ->
        let oldChild = elems.(idx) in
        let newChild = create_element callbacks newNode in
        DomNode.replace elem oldChild newChild;
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest

  let patch_nodes_into_element callbacks elem oldVNodes newVNodes =
    let elems = Web.Node.child_nodes elem in
    patch_nodes callbacks elem elems 0 oldVNodes newVNodes;
    newVNodes

  let patch_node_into_element callbacks elem oldVNode newVNode =
    patch_nodes_into_element callbacks elem [ oldVNode ] [ newVNode ]
end

let map : ('a -> 'b) -> 'a Node.t -> 'b Node.t =
 fun func vdom ->
  let tagger = ApplicationCallbacks.wrap_callbacks func in
  Tagger (Obj.magic tagger, Obj.magic vdom)
