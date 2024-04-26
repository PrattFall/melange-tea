type 'msg systemMessage =
  | Render
  | AddRenderMsg of 'msg
  | RemoveRenderMsg of 'msg

type 'msg applicationCallbacks = {
  enqueue : 'msg -> unit;
  on : 'msg systemMessage -> unit;
}

type 'msg eventHandler =
  | EventHandlerCallback of string * (Dom.event -> 'msg option)
  | EventHandlerMsg of 'msg

type 'msg eventCache = {
  handler : Dom.event -> unit;
  cb : (Dom.event -> 'msg option) ref;
}

module Property = struct
  type 'msg t =
    | NoProp
    | RawProp of string * string
    (* namespace, key, value *)
    | Attribute of string * string * string
    | Data of string * string
    | Event of string * 'msg eventHandler * 'msg eventCache option ref
    | Style of (string * string) list

  let noProp = NoProp
  let prop key value = RawProp (key, value)
  let onCB name key cb = Event (name, EventHandlerCallback (key, cb), ref None)
  let onMsg name msg = Event (name, EventHandlerMsg msg, ref None)
  let attribute namespace key value = Attribute (namespace, key, value)
  let data key value = Data (key, value)
  let style key value = Style [ (key, value) ]
  let styles s = Style s
  let mapEmpty props = List.map (fun _ -> noProp) props

  let render = function
    | NoProp -> ""
    | RawProp (k, v) -> String.concat "" [ " "; k; "=\""; v; "\"" ]
    | Attribute (_, k, v) -> String.concat "" [ " "; k; "=\""; v; "\"" ]
    | Data (k, v) -> String.concat "" [ " data-"; k; "=\""; v; "\"" ]
    | Event (_, _, _) -> ""
    | Style s ->
        String.concat ""
          [
            " style=\"";
            String.concat ";"
              (List.map (fun (k, v) -> String.concat "" [ k; ":"; v; ";" ]) s);
            "\"";
          ]
end

type 'msg properties = 'msg Property.t list

type 'msg t =
  | CommentNode of string
  | Text of string
  (* namespace, tagName, key, unique, props, vdoms *)
  | Node of string * string * string * string * 'msg properties * 'msg t list
  (* key, gen, cache *)
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  (* tagger, vdom *)
  | Tagger of
      ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t

let noNode = CommentNode ""
let comment s = CommentNode s
let text s = Text s

let fullnode namespace tagName key unique props vdoms =
  Node (namespace, tagName, key, unique, props, vdoms)

let node ?(namespace = "") ?(key = "") ?(unique = "") tagName props vdoms =
  fullnode namespace tagName key unique props vdoms

let lazyGen key fn = LazyGen (key, fn, ref noNode)

let rec renderToHtmlString = function
  | CommentNode s -> "<!-- " ^ s ^ " -->"
  | Text s -> s
  | Node (namespace, tagName, _, _, props, vdoms) ->
      String.concat ""
        [
          "<";
          namespace;
          (if namespace = "" then "" else ":");
          tagName;
          String.concat "" (List.map Property.render props);
          ">";
          String.concat "" (List.map renderToHtmlString vdoms);
          "</";
          tagName;
          ">";
        ]
  | LazyGen (_, gen, _) -> renderToHtmlString (gen ())
  | Tagger (_, vdom) -> renderToHtmlString vdom

let emptyEventCB _ev : 'e Web_event.callback option = None
let eventHandler callbacks cb ev = Option.iter !callbacks.enqueue (!cb ev)

let eventHandler_GetCB = function
  | EventHandlerCallback (_, cb) -> cb
  | EventHandlerMsg (msg : 'msg) -> fun _ev -> Some msg

let compareEventHandlerTypes (left : 'msg eventHandler) :
    'msg eventHandler -> bool = function
  | EventHandlerCallback (cb, _) -> (
      match left with
      | EventHandlerCallback (lcb, _) when cb = lcb -> true
      | _ -> false)
  | EventHandlerMsg msg -> (
      match left with
      | EventHandlerMsg lmsg when msg = lmsg -> true
      | _ -> false)

let eventHandler_Register callbacks elem name handlerType =
  let cb = ref (eventHandler_GetCB handlerType) in
  let handler = eventHandler callbacks cb in
  Web_node.add_event_listener elem name handler;
  Some { handler; cb }

let eventHandler_Unregister (elem : 'a Dom.node_like) (event_name : string) :
    'msg eventCache option -> 'msg eventCache option = function
  | None -> None
  | Some cache ->
      Web_node.remove_event_listener elem event_name cache.handler;
      None

let eventHandler_Mutate callbacks elem oldName newName oldHandlerType
    newHandlerType oldCache newCache =
  match !oldCache with
  | None ->
      newCache := eventHandler_Register callbacks elem newName newHandlerType
  | Some oldcache ->
      if oldName = newName then (
        newCache := !oldCache;
        if compareEventHandlerTypes oldHandlerType newHandlerType then ()
        else oldcache.cb := eventHandler_GetCB newHandlerType)
      else (
        oldCache := eventHandler_Unregister elem oldName !oldCache;
        newCache := eventHandler_Register callbacks elem newName newHandlerType)

let patchVNodesOnElems_PropertiesApply_Add callbacks elem = function
  | Property.NoProp -> ()
  | Property.RawProp (k, v) -> Web_node.set_prop elem k v
  | Property.Attribute (namespace, k, v) ->
      Web_node.set_attribute ~namespace elem k v
  | Property.Data (k, v) ->
      Js.log ("TODO:  Add Data Unhandled", k, v);
      failwith "TODO:  Add Data Unhandled"
  | Property.Event (name, handlerType, cache) ->
      cache := eventHandler_Register callbacks elem name handlerType
  | Property.Style s ->
      List.fold_left
        (fun () (k, v) -> Web_node.set_style_property elem k (Js.Null.return v))
        () s

let patchVNodesOnElems_PropertiesApply_Remove elem = function
  | Property.NoProp -> ()
  | Property.RawProp (k, _) -> Web_node.set_prop elem k Js.Undefined.empty
  | Property.Attribute (namespace, k, _) ->
      Web_node.remove_attribute ~namespace elem k
  | Property.Data (k, v) ->
      Js.log ("TODO:  Remove Data Unhandled", k, v);
      failwith "TODO:  Remove Data Unhandled"
  | Property.Event (name, _, cache) ->
      cache := eventHandler_Unregister elem name !cache
  | Property.Style s ->
      List.fold_left
        (fun () (k, _) -> Web_node.set_style_property elem k Js.Null.empty)
        () s

let patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem oldProp newProp
    =
  patchVNodesOnElems_PropertiesApply_Remove elem oldProp;
  patchVNodesOnElems_PropertiesApply_Add callbacks elem newProp

let patchVNodesOnElems_PropertiesApply_Mutate elem oldProp = function
  | Property.NoProp ->
      failwith
        "This should never be called as all entries through NoProp are gated."
  | Property.RawProp (k, v) -> Web_node.set_prop elem k v
  | Property.Attribute (namespace, k, v) ->
      Web_node.set_attribute ~namespace elem k v
  | Property.Data (k, v) ->
      Js.log ("TODO:  Mutate Data Unhandled", k, v);
      failwith "TODO:  Mutate Data Unhandled"
  | Property.Event (_, _, _) ->
      failwith "This will never be called because it is gated"
  | Property.Style s -> (
      match oldProp with
      | Property.Style oldS ->
          List.fold_left2
            (fun () (ok, ov) (nk, nv) ->
              if ok = nk then
                if ov = nv then ()
                else Web_node.set_style_property elem nk (Js.Null.return nv)
              else (
                Web_node.set_style_property elem ok Js.Null.empty;
                Web_node.set_style_property elem nk (Js.Null.return nv)))
            () oldS s
      | _ ->
          failwith
            "Passed a non-Style to a new Style as a Mutations while the old \
             Style is not actually a style!")

let rec patchVNodesOnElems_PropertiesApply callbacks elem idx oldProperties
    newProperties =
  let open Property in
  match (oldProperties, newProperties) with
  | [], [] -> true
  | [], _ :: _ -> false
  | _ :: _, [] -> false
  | NoProp :: oldRest, NoProp :: newRest ->
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (RawProp (oldK, oldV) as oldProp) :: oldRest,
      (RawProp (newK, newV) as newProp) :: newRest ) ->
      let () =
        if oldK = newK && oldV = newV then ()
        else patchVNodesOnElems_PropertiesApply_Mutate elem oldProp newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (Attribute (oldNS, oldK, oldV) as oldProp) :: oldRest,
      (Attribute (newNS, newK, newV) as newProp) :: newRest ) ->
      let () =
        if oldNS = newNS && oldK = newK && oldV = newV then ()
        else patchVNodesOnElems_PropertiesApply_Mutate elem oldProp newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (Data (oldK, oldV) as oldProp) :: oldRest,
      (Data (newK, newV) as newProp) :: newRest ) ->
      let () =
        if oldK = newK && oldV = newV then ()
        else patchVNodesOnElems_PropertiesApply_Mutate elem oldProp newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( Event (oldName, oldHandlerType, oldCache) :: oldRest,
      Event (newName, newHandlerType, newCache) :: newRest ) ->
      let () =
        eventHandler_Mutate callbacks elem oldName newName oldHandlerType
          newHandlerType oldCache newCache
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | (Style oldS as oldProp) :: oldRest, (Style newS as newProp) :: newRest ->
      let () =
        if oldS = newS then ()
        else patchVNodesOnElems_PropertiesApply_Mutate elem oldProp newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | oldProp :: oldRest, newProp :: newRest ->
      let () =
        patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem oldProp
          newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest

let patchVNodesOnElems_Properties callbacks elem oldProperties newProperties =
  patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties
    newProperties

let genEmptyProps length =
  let rec aux lst = function
    | 0 -> lst
    | len -> aux (Property.noProp :: lst) (len - 1)
  in
  aux [] length

let rec patchVNodesOnElems_ReplaceNode callbacks elem elems idx = function
  | Node (newNamespace, newTagName, _, _, newProperties, newChildren) ->
      let oldChild = elems.(idx) in

      let newChild =
        Web.Document.create_element ~namespace:newNamespace newTagName
      in

      let _ =
        patchVNodesOnElems_Properties callbacks newChild
          (Property.mapEmpty newProperties)
          newProperties
      in

      let childChildren = Web_node.child_nodes newChild in

      patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren;
      ignore (Web_node.insert_before elem newChild oldChild);
      ignore (Web_node.remove_child elem oldChild)
  | _ ->
      failwith
        "Node replacement should never be passed anything but a node itself"

and patchVNodesOnElems_CreateElement callbacks = function
  | CommentNode s -> Web.Document.create_comment s
  | Text text -> Web.Document.create_text_node text
  | Node (newNamespace, newTagName, _, _, newProperties, newChildren) ->
      let newChild =
        Web.Document.create_element ~namespace:newNamespace newTagName
      in

      ignore
        (patchVNodesOnElems_Properties callbacks newChild
           (Property.mapEmpty newProperties)
           newProperties);

      let childChildren = Web_node.child_nodes newChild in
      patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren;
      newChild
  | LazyGen (_, newGen, newCache) ->
      let vdom = newGen () in
      newCache := vdom;
      patchVNodesOnElems_CreateElement callbacks vdom
  | Tagger (tagger, vdom) ->
      patchVNodesOnElems_CreateElement (tagger callbacks) vdom

and patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode =
  match (oldNode, newNode) with
  | ( Node (_, oldTagName, _, oldUnique, oldProperties, oldChildren),
      (Node (_, newTagName, _, newUnique, newProperties, newChildren) as newNode)
    ) ->
      if oldUnique <> newUnique || oldTagName <> newTagName then
        patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
      else
        let child = elems.(idx) in
        let childChildren = Web_node.child_nodes child in
        let () =
          if
            patchVNodesOnElems_Properties callbacks child oldProperties
              newProperties
          then ()
          else
            let () =
              Js.log
                "VDom:  Failed swapping properties because the property list \
                 length changed, use `noProp` to swap properties instead, not \
                 by altering the list structure.  This is a massive \
                 inefficiency until this issue is resolved."
            in
            patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
        in
        patchVNodesOnElems callbacks child childChildren 0 oldChildren
          newChildren
  | _ -> failwith "Non-node passed to patchVNodesOnElems_MutateNode"

and patchVNodesOnElems callbacks elem elems idx oldVNodes newVNodes =
  match (oldVNodes, newVNodes) with
  | Tagger (_, oldVdom) :: oldRest, _ ->
      patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) newVNodes
  | oldNode :: oldRest, Tagger (newTagger, newVdom) :: newRest ->
      let () =
        patchVNodesOnElems (newTagger callbacks) elem elems idx [ oldNode ]
          [ newVdom ]
      in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | [], [] -> ()
  | [], newNode :: newRest ->
      let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
      ignore (Web_node.append_child elem newChild);
      patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
  | _ :: oldRest, [] ->
      let child = elems.(idx) in
      ignore (Web_node.remove_child elem child);
      patchVNodesOnElems callbacks elem elems idx oldRest []
  | CommentNode oldS :: oldRest, CommentNode newS :: newRest when oldS = newS ->
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | Text oldText :: oldRest, Text newText :: newRest ->
      (if oldText = newText then ()
       else
         let child = elems.(idx) in
         Web_node.set_value child newText);
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | ( LazyGen (oldKey, _oldGen, oldCache) :: oldRest,
      LazyGen (newKey, newGen, newCache) :: newRest ) -> (
      if oldKey = newKey then (
        newCache := !oldCache;
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest)
      else
        match (oldRest, newRest) with
        | ( LazyGen (olderKey, _, _) :: olderRest,
            LazyGen (newerKey, _, _) :: newerRest )
          when olderKey = newKey && oldKey = newerKey ->
            let firstChild = elems.(idx) in
            let secondChild = elems.(idx + 1) in
            ignore (Web_node.remove_child elem secondChild);
            ignore (Web_node.insert_before elem secondChild firstChild);
            patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
              newerRest
        | LazyGen (olderKey, _, olderCache) :: olderRest, _
          when olderKey = newKey ->
            let oldChild = elems.(idx) in
            ignore (Web_node.remove_child elem oldChild);
            let oldVdom = !olderCache in
            newCache := oldVdom;
            patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
        | _, LazyGen (newerKey, _, _) :: _ when newerKey = oldKey ->
            let oldChild = elems.(idx) in
            let newVdom = newGen () in
            newCache := newVdom;
            let newChild = patchVNodesOnElems_CreateElement callbacks newVdom in
            ignore (Web_node.insert_before elem newChild oldChild);
            patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
        | _ ->
            let oldVdom = !oldCache in
            let newVdom = newGen () in
            newCache := newVdom;
            patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest)
              (newVdom :: newRest))
  | ( (Node (oldNamespace, oldTagName, oldKey, _, _, _) as oldNode) :: oldRest,
      (Node (newNamespace, newTagName, newKey, _, _, _) as newNode) :: newRest )
    -> (
      if oldKey = newKey && oldKey <> "" then
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else if oldKey = "" || newKey = "" then (
        patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode;
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest)
      else
        match (oldRest, newRest) with
        | ( Node (olderNamespace, olderTagName, olderKey, _, _, _) :: olderRest,
            Node (newerNamespace, newerTagName, newerKey, _, _, _) :: newerRest
          )
          when olderNamespace = newNamespace
               && olderTagName = newTagName && olderKey = newKey
               && oldNamespace = newerNamespace
               && oldTagName = newerTagName && oldKey = newerKey ->
            let firstChild = elems.(idx) in
            let secondChild = elems.(idx + 1) in
            ignore (Web_node.remove_child elem secondChild);
            ignore (Web_node.insert_before elem secondChild firstChild);
            patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
              newerRest
        | Node (olderNamespace, olderTagName, olderKey, _, _, _) :: olderRest, _
          when olderNamespace = newNamespace
               && olderTagName = newTagName && olderKey = newKey ->
            let oldChild = elems.(idx) in
            ignore (Web_node.remove_child elem oldChild);
            patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
        | _, Node (newerNamespace, newerTagName, newerKey, _, _, _) :: _
          when oldNamespace = newerNamespace
               && oldTagName = newerTagName && oldKey = newerKey ->
            let oldChild = elems.(idx) in
            let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
            let _attachedChild =
              Web_node.insert_before elem newChild oldChild
            in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
        | _ ->
            patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode
              newNode;
            patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest)
  | _ :: oldRest, newNode :: newRest ->
      let oldChild = elems.(idx) in
      let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
      ignore (Web_node.insert_before elem newChild oldChild);
      ignore (Web_node.remove_child elem oldChild);
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest

let patchVNodesIntoElement callbacks elem oldVNodes newVNodes =
  let elems = Web_node.child_nodes elem in
  patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes;
  newVNodes

let patchVNodeIntoElement callbacks elem oldVNode newVNode =
  patchVNodesIntoElement callbacks elem [ oldVNode ] [ newVNode ]

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

let map =
  ((fun func vdom ->
      let tagger = wrapCallbacks func in
      Tagger (Obj.magic tagger, Obj.magic vdom)
     : ('a -> 'b) -> 'a t -> 'b t)
    : ('a -> 'b) -> 'a t -> 'b t)
