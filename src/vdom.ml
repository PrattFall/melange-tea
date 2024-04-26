type 'msg systemMessage =
  | Render
  | AddRenderMsg of 'msg
  | RemoveRenderMsg of 'msg

type 'msg applicationCallbacks = {
  enqueue : 'msg -> unit;
  on : 'msg systemMessage -> unit;
}

module EventHandler = struct
  type 'msg t =
    | EventHandlerCallback of string * (Dom.event -> 'msg option)
    | EventHandlerMsg of 'msg

  type 'msg cache = {
    handler : Dom.event -> unit;
    cb : (Dom.event -> 'msg option) ref;
  }

  let emptyEventCB _ev : 'e Web_event.callback option = None
  let eventHandler callbacks cb ev = Option.iter !callbacks.enqueue (!cb ev)

  let get_callback = function
    | EventHandlerCallback (_, cb) -> cb
    | EventHandlerMsg (msg : 'msg) -> fun _ev -> Some msg

  let compareEventHandlerTypes left right =
    match (left, right) with
    | EventHandlerCallback (lcb, _), EventHandlerCallback (rcb, _) -> lcb = rcb
    | EventHandlerMsg lmsg, EventHandlerMsg rmsg -> lmsg = rmsg
    | _ -> false

  let register callbacks elem name handlerType =
    let cb = ref (get_callback handlerType) in
    let handler = eventHandler callbacks cb in
    Web_node.add_event_listener elem name handler;
    Some { handler; cb }

  let unregister (elem : 'a Dom.node_like) (event_name : string) :
      'msg cache option -> 'msg cache option = function
    | None -> None
    | Some cache ->
        Web_node.remove_event_listener elem event_name cache.handler;
        None

  let mutate callbacks elem oldName newName oldHandlerType newHandlerType
      oldCache newCache =
    match !oldCache with
    | None -> newCache := register callbacks elem newName newHandlerType
    | Some oldcache ->
        if oldName = newName then (
          newCache := !oldCache;
          if compareEventHandlerTypes oldHandlerType newHandlerType then ()
          else oldcache.cb := get_callback newHandlerType)
        else (
          oldCache := unregister elem oldName !oldCache;
          newCache := register callbacks elem newName newHandlerType)
end

module Property = struct
  type 'msg t =
    | NoProp
    | RawProp of string * string
    (* namespace, key, value *)
    | Attribute of string * string * string
    | Data of string * string
    | Event of string * 'msg EventHandler.t * 'msg EventHandler.cache option ref
    | Style of (string * string) list

  let empty = NoProp
  let prop key value = RawProp (key, value)
  let onCB name key cb = Event (name, EventHandlerCallback (key, cb), ref None)
  let onMsg name msg = Event (name, EventHandlerMsg msg, ref None)
  let attribute namespace key value = Attribute (namespace, key, value)
  let data key value = Data (key, value)
  let style key value = Style [ (key, value) ]
  let styles s = Style s
  let mapEmpty props = List.map (fun _ -> empty) props

  let to_string = function
    | RawProp (k, v) -> String.concat "" [ " "; k; "=\""; v; "\"" ]
    | Attribute (_, k, v) -> String.concat "" [ " "; k; "=\""; v; "\"" ]
    | Data (k, v) -> String.concat "" [ " data-"; k; "=\""; v; "\"" ]
    | Style s ->
        String.concat ""
          [
            " style=\"";
            String.concat ";"
              (List.map (fun (k, v) -> String.concat "" [ k; ":"; v; ";" ]) s);
            "\"";
          ]
    | _ -> ""

  let genEmpty length =
    let rec aux lst = function
      | 0 -> lst
      | len -> aux (empty :: lst) (len - 1)
    in
    aux [] length

  let apply_to_element callbacks elem = function
    | NoProp -> ()
    | RawProp (k, v) -> Web_node.set_prop elem k v
    | Attribute (namespace, k, v) -> Web_node.set_attribute ~namespace elem k v
    | Data (k, v) ->
        Js.log ("TODO:  Add Data Unhandled", k, v);
        failwith "TODO:  Add Data Unhandled"
    | Event (name, handlerType, cache) ->
        cache := EventHandler.register callbacks elem name handlerType
    | Style s ->
        List.iter (fun (k, v) -> Web_node.set_style_property elem k (Some v)) s

  let remove_from_element elem = function
    | NoProp -> ()
    | RawProp (k, _) -> Web_node.set_prop elem k Js.Undefined.empty
    | Attribute (namespace, k, _) -> Web_node.remove_attribute ~namespace elem k
    | Data (k, v) ->
        Js.log ("TODO:  Remove Data Unhandled", k, v);
        failwith "TODO:  Remove Data Unhandled"
    | Event (name, _, cache) ->
        cache := EventHandler.unregister elem name !cache
    | Style s ->
        List.iter (fun (k, _) -> Web_node.set_style_property elem k None) s

  let replace_on_element callbacks elem oldProp newProp =
    remove_from_element elem oldProp;
    apply_to_element callbacks elem newProp

  let mutate_on_element elem oldProp = function
    | NoProp ->
        failwith
          "This should never be called as all entries through NoProp are gated."
    | RawProp (k, v) -> Web_node.set_prop elem k v
    | Attribute (namespace, k, v) -> Web_node.set_attribute ~namespace elem k v
    | Data (k, v) ->
        Js.log ("TODO:  Mutate Data Unhandled", k, v);
        failwith "TODO:  Mutate Data Unhandled"
    | Event (_, _, _) ->
        failwith "This will never be called because it is gated"
    | Style newStyles -> (
        match oldProp with
        | Style oldStyles ->
            (* Works for now  *)
            List.iter
              (fun (key, _) -> Web.Node.set_style_property elem key None)
              oldStyles;
            List.iter
              (fun (key, value) ->
                Web.Node.set_style_property elem key (Some value))
              newStyles
        | _ ->
            failwith
              "Passed a non-Style to a new Style as a Mutations while the old \
               Style is not actually a style!")

  let rec apply callbacks elem idx oldProperties newProperties =
    match (oldProperties, newProperties) with
    | [], [] -> true
    | [], _ :: _ -> false
    | _ :: _, [] -> false
    | NoProp :: oldRest, NoProp :: newRest ->
        apply callbacks elem (idx + 1) oldRest newRest
    | ( (RawProp (oldK, oldV) as oldProp) :: oldRest,
        (RawProp (newK, newV) as newProp) :: newRest ) ->
        if oldK = newK && oldV = newV then ()
        else mutate_on_element elem oldProp newProp;
        apply callbacks elem (idx + 1) oldRest newRest
    | ( (Attribute (oldNS, oldK, oldV) as oldProp) :: oldRest,
        (Attribute (newNS, newK, newV) as newProp) :: newRest ) ->
        if oldNS = newNS && oldK = newK && oldV = newV then ()
        else mutate_on_element elem oldProp newProp;
        apply callbacks elem (idx + 1) oldRest newRest
    | ( (Data (oldK, oldV) as oldProp) :: oldRest,
        (Data (newK, newV) as newProp) :: newRest ) ->
        if oldK = newK && oldV = newV then ()
        else mutate_on_element elem oldProp newProp;
        apply callbacks elem (idx + 1) oldRest newRest
    | ( Event (oldName, oldHandlerType, oldCache) :: oldRest,
        Event (newName, newHandlerType, newCache) :: newRest ) ->
        EventHandler.mutate callbacks elem oldName newName oldHandlerType
          newHandlerType oldCache newCache;
        apply callbacks elem (idx + 1) oldRest newRest
    | (Style oldS as oldProp) :: oldRest, (Style newS as newProp) :: newRest ->
        if oldS = newS then () else mutate_on_element elem oldProp newProp;
        apply callbacks elem (idx + 1) oldRest newRest
    | oldProp :: oldRest, newProp :: newRest ->
        replace_on_element callbacks elem oldProp newProp;
        apply callbacks elem (idx + 1) oldRest newRest
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
end

module LazyGen = struct
  type 'container t = {
    key : string;
    gen : unit -> 'container;
    cache : 'container ref;
  }

  let make key gen cache = { key; gen; cache; }
end

module Node = struct
  type 'msg t =
    | CommentNode of string
    | Text of string
    | Node of ('msg, 'msg t) DomNode.t
    (* key, gen, cache *)
    | LazyGen of string * (unit -> 'msg t) * 'msg t ref
    (* tagger, vdom *)
    | Tagger of
        ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref)
        * 'msg t

  let empty = CommentNode ""
  let comment s = CommentNode s
  let text s = Text s

  let fullnode namespace tag_name key unique props vdoms =
    Node (DomNode.make ~namespace ~key ~unique tag_name props vdoms)

  let node ?(namespace = "") ?(key = "") ?(unique = "") tagName props vdoms =
    fullnode namespace tagName key unique props vdoms

  let lazyGen key fn = LazyGen (key, fn, ref empty)

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
    | LazyGen (_, gen, _) -> to_string (gen ())
    | Tagger (_, vdom) -> to_string vdom

  let apply_properties callbacks elem oldProperties newProperties =
    Property.apply callbacks elem 0 oldProperties newProperties

  let init_properties callbacks newChild newProperties =
    ignore
      (apply_properties callbacks newChild
         (Property.mapEmpty newProperties)
         newProperties)

  let rec create_dom_node callbacks newNode =
    let open DomNode in
    let newChild =
      Web.Document.create_element ~namespace:newNode.namespace newNode.tag_name
    in

    init_properties callbacks newChild newNode.props;

    let grandChildren = Web_node.child_nodes newChild in

    patch_nodes callbacks newChild grandChildren 0 [] newNode.vdoms;

    newChild

  and replace callbacks elem elems idx = function
    | Node newNode ->
        let oldChild = elems.(idx) in
        let newChild = create_dom_node callbacks newNode in
        ignore (Web_node.insert_before elem newChild oldChild);
        ignore (Web_node.remove_child elem oldChild)
    | _ ->
        failwith
          "Node replacement should never be passed anything but a node itself"

  and create_element callbacks = function
    | CommentNode s -> Web.Document.create_comment s
    | Text text -> Web.Document.create_text_node text
    | Node newNode -> create_dom_node callbacks newNode
    | LazyGen (_, newGen, newCache) ->
        let vdom = newGen () in
        newCache := vdom;
        create_element callbacks vdom
    | Tagger (tagger, vdom) -> create_element (tagger callbacks) vdom

  and mutate callbacks elem elems idx oldNode newNode =
    match (oldNode, newNode) with
    | Node old, Node nw ->
        if old.unique <> nw.unique || old.tag_name <> nw.tag_name then
          replace callbacks elem elems idx newNode
        else
          let child = elems.(idx) in
          let grandChildren = Web_node.child_nodes child in
          if apply_properties callbacks child old.props nw.props then ()
          else (
            Js.log
              "VDom:  Failed swapping properties because the property list \
               length changed, use `noProp` to swap properties instead, not by \
               altering the list structure.  This is a massive inefficiency \
               until this issue is resolved.";
            replace callbacks elem elems idx newNode);
          patch_nodes callbacks child grandChildren 0 old.vdoms nw.vdoms
    | _ -> failwith "Non-node passed to mutate"

  and patch_nodes callbacks elem elems idx oldVNodes newVNodes =
    match (oldVNodes, newVNodes) with
    | Tagger (_, oldVdom) :: oldRest, _ ->
        patch_nodes callbacks elem elems idx (oldVdom :: oldRest) newVNodes
    | oldNode :: oldRest, Tagger (newTagger, newVdom) :: newRest ->
        patch_nodes (newTagger callbacks) elem elems idx [ oldNode ] [ newVdom ];
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest
    | [], [] -> ()
    | [], newNode :: newRest ->
        let newChild = create_element callbacks newNode in
        ignore (Web_node.append_child elem newChild);
        patch_nodes callbacks elem elems (idx + 1) [] newRest
    | _ :: oldRest, [] ->
        let child = elems.(idx) in
        ignore (Web_node.remove_child elem child);
        patch_nodes callbacks elem elems idx oldRest []
    | CommentNode oldS :: oldRest, CommentNode newS :: newRest when oldS = newS
      ->
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest
    | Text oldText :: oldRest, Text newText :: newRest ->
        (if oldText = newText then ()
         else
           let child = elems.(idx) in
           Web_node.set_value child newText);
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest
    | ( LazyGen (oldKey, _, oldCache) :: oldRest,
        LazyGen (newKey, newGen, newCache) :: newRest ) -> (
        if oldKey = newKey then (
          newCache := !oldCache;
          patch_nodes callbacks elem elems (idx + 1) oldRest newRest)
        else
          match (oldRest, newRest) with
          | ( LazyGen (olderKey, _, _) :: olderRest,
              LazyGen (newerKey, _, _) :: newerRest )
            when olderKey = newKey && oldKey = newerKey ->
              let firstChild = elems.(idx) in
              let secondChild = elems.(idx + 1) in
              ignore (Web_node.remove_child elem secondChild);
              ignore (Web_node.insert_before elem secondChild firstChild);
              patch_nodes callbacks elem elems (idx + 2) olderRest newerRest
          | LazyGen (olderKey, _, olderCache) :: olderRest, _
            when olderKey = newKey ->
              let oldChild = elems.(idx) in
              ignore (Web_node.remove_child elem oldChild);
              let oldVdom = !olderCache in
              newCache := oldVdom;
              patch_nodes callbacks elem elems (idx + 1) olderRest newRest
          | _, LazyGen (newerKey, _, _) :: _ when newerKey = oldKey ->
              let oldChild = elems.(idx) in
              let newVdom = newGen () in
              newCache := newVdom;
              let newChild = create_element callbacks newVdom in
              ignore (Web_node.insert_before elem newChild oldChild);
              patch_nodes callbacks elem elems (idx + 1) oldVNodes newRest
          | _ ->
              let oldVdom = !oldCache in
              let newVdom = newGen () in
              newCache := newVdom;
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
              ignore (Web_node.remove_child elem secondChild);
              ignore (Web_node.insert_before elem secondChild firstChild);
              patch_nodes callbacks elem elems (idx + 2) olderRest newerRest
          | Node olderNode :: olderRest, _ when DomNode.equals olderNode newNode
            ->
              let oldChild = elems.(idx) in
              ignore (Web_node.remove_child elem oldChild);
              patch_nodes callbacks elem elems (idx + 1) olderRest newRest
          | _, Node newerNode :: _ when DomNode.equals oldNode newerNode ->
              let oldChild = elems.(idx) in
              let newChild = create_element callbacks (Node newNode) in
              let _attachedChild =
                Web_node.insert_before elem newChild oldChild
              in
              patch_nodes callbacks elem elems (idx + 1) oldVNodes newRest
          | _ ->
              mutate callbacks elem elems idx (Node oldNode) (Node newNode);
              patch_nodes callbacks elem elems (idx + 1) oldRest newRest)
    | _ :: oldRest, newNode :: newRest ->
        let oldChild = elems.(idx) in
        let newChild = create_element callbacks newNode in
        ignore (Web_node.insert_before elem newChild oldChild);
        ignore (Web_node.remove_child elem oldChild);
        patch_nodes callbacks elem elems (idx + 1) oldRest newRest

  let patch_nodes_into_element callbacks elem oldVNodes newVNodes =
    let elems = Web_node.child_nodes elem in
    patch_nodes callbacks elem elems 0 oldVNodes newVNodes;
    newVNodes

  let patch_node_into_element callbacks elem oldVNode newVNode =
    patch_nodes_into_element callbacks elem [ oldVNode ] [ newVNode ]
end

let wrapCallbacks_On : type a b. (a -> b) -> a systemMessage -> b systemMessage
    =
 fun func -> function
  | Render -> Render
  | AddRenderMsg msg -> AddRenderMsg (func msg)
  | RemoveRenderMsg msg -> RemoveRenderMsg (func msg)

let wrapCallbacks :
    type a b.
    (a -> b) -> b applicationCallbacks ref -> a applicationCallbacks ref =
 fun func callbacks ->
  Obj.magic ref
    {
      enqueue =
        (fun (msg : a) ->
          let new_msg = func msg in
          !callbacks.enqueue new_msg);
      on =
        (fun smsg ->
          let new_smsg = wrapCallbacks_On func smsg in
          !callbacks.on new_smsg);
    }

let map : ('a -> 'b) -> 'a Node.t -> 'b Node.t =
 fun func vdom ->
  let tagger = wrapCallbacks func in
  Tagger (Obj.magic tagger, Obj.magic vdom)
