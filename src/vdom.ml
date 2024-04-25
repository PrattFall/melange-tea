open Web_node

type 'msg systemMessage =
  | Render
  | AddRenderMsg of 'msg
  | RemoveRenderMsg of 'msg

type 'msg applicationCallbacks = {
  enqueue : 'msg -> unit;
  on : 'msg systemMessage -> unit;
}

type 'msg eventHandler =
  | EventHandlerCallback of string * (dom_event -> 'msg option)
  | EventHandlerMsg of 'msg

type 'msg eventCache = {
  handler : Dom.element Web_event.cb;
  cb : (dom_event -> 'msg option) ref;
}

type 'msg property =
  | NoProp
  | RawProp of string * string
  | Attribute of string * string * string
  | Data of string * string
  | Event of string * 'msg eventHandler * 'msg eventCache option ref
  | Style of (string * string) list

type 'msg properties = 'msg property list

type 'msg t =
  | CommentNode of string
  | Text of string
  | Node of string * string * string * string * 'msg properties * 'msg t list
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  | Tagger of
      ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t

let noNode = (((CommentNode "" [@explicit_arity]) : 'msg t) : 'msg t)
let comment (s : string) : 'msg t = (CommentNode s [@explicit_arity])
let text (s : string) : 'msg t = (Text s [@explicit_arity])

let fullnode (namespace : string) (tagName : string) (key : string)
    (unique : string) (props : 'msg properties) (vdoms : 'msg t list) : 'msg t =
  (Node (namespace, tagName, key, unique, props, vdoms) [@implicit_arity])

let node ?(namespace : string = "") (tagName : string) ?(key : string = "")
    ?(unique : string = "") (props : 'msg properties) (vdoms : 'msg t list) :
    'msg t =
  fullnode namespace tagName key unique props vdoms

let lazyGen (key : string) (fn : unit -> 'msg t) : 'msg t =
  (LazyGen (key, fn, ref noNode) [@implicit_arity])

let noProp = ((NoProp : 'msg property) : 'msg property)

let prop (key : string) (value : string) : 'msg property =
  (RawProp (key, value) [@implicit_arity])

let onCB (name : string) (key : string) (cb : dom_event -> 'msg option) :
    'msg property =
  (Event (name, (EventHandlerCallback (key, cb) [@implicit_arity]), ref None)
  [@implicit_arity])

let onMsg (name : string) (msg : 'msg) : 'msg property =
  (Event (name, (EventHandlerMsg msg [@explicit_arity]), ref None)
  [@implicit_arity])

let attribute (namespace : string) (key : string) (value : string) :
    'msg property =
  (Attribute (namespace, key, value) [@implicit_arity])

let data (key : string) (value : string) : 'msg property =
  (Data (key, value) [@implicit_arity])

let style (key : string) (value : string) : 'msg property =
  (Style [ (key, value) ] [@explicit_arity])

let styles s : 'msg property = (Style s [@explicit_arity])

let rec renderToHtmlString =
  ((function
    | ((CommentNode s) [@explicit_arity]) -> "<!-- " ^ s ^ " -->"
    | ((Text s) [@explicit_arity]) -> s
    | ((Node (namespace, tagName, _key, _unique, props, vdoms))
    [@implicit_arity]) ->
        let renderProp = function
          | NoProp -> ""
          | ((RawProp (k, v)) [@implicit_arity]) ->
              String.concat "" [ " "; k; "=\""; v; "\"" ]
          | ((Attribute (_namespace, k, v)) [@implicit_arity]) ->
              String.concat "" [ " "; k; "=\""; v; "\"" ]
          | ((Data (k, v)) [@implicit_arity]) ->
              String.concat "" [ " data-"; k; "=\""; v; "\"" ]
          | ((Event (_, _, _)) [@implicit_arity]) -> ""
          | ((Style s) [@explicit_arity]) ->
              String.concat ""
                [
                  " style=\"";
                  String.concat ";"
                    (List.map
                       (fun (k, v) -> String.concat "" [ k; ":"; v; ";" ])
                       s);
                  "\"";
                ]
        in
        String.concat ""
          [
            "<";
            namespace;
            (if namespace = "" then "" else ":");
            tagName;
            String.concat "" (List.map (fun p -> renderProp p) props);
            ">";
            String.concat "" (List.map (fun v -> renderToHtmlString v) vdoms);
            "</";
            tagName;
            ">";
          ]
    | ((LazyGen (_key, gen, _cache)) [@implicit_arity]) ->
        let vdom = gen () in
        renderToHtmlString vdom
    | ((Tagger (_tagger, vdom)) [@implicit_arity]) -> renderToHtmlString vdom
     : 'msg t -> string)
    : 'msg t -> string)

let emptyEventHandler = ((fun [@bs] _ev -> () : dom_event_cb) : dom_event_cb)
let emptyEventCB _ev : dom_event_cb option = None

let eventHandler (callbacks : 'msg applicationCallbacks ref)
    (cb : (dom_event -> 'msg option) ref) : dom_event_cb =
 fun [@bs] ev ->
  match !cb ev with
  | None -> ()
  | ((Some msg) [@explicit_arity]) -> !callbacks.enqueue msg

let eventHandler_GetCB =
  ((function
    | ((EventHandlerCallback (_, cb)) [@implicit_arity]) -> cb
    | ((EventHandlerMsg msg) [@explicit_arity]) ->
        fun _ev -> (Some msg [@explicit_arity])
     : 'msg eventHandler -> dom_event -> 'msg option)
    : 'msg eventHandler -> dom_event -> 'msg option)

let compareEventHandlerTypes (left : 'msg eventHandler) :
    'msg eventHandler -> bool = function
  | ((EventHandlerCallback (cb, _)) [@implicit_arity]) -> (
      match left with
      | ((EventHandlerCallback (lcb, _)) [@implicit_arity]) when cb = lcb ->
          true
      | _ -> false)
  | ((EventHandlerMsg msg) [@explicit_arity]) -> (
      match left with
      | ((EventHandlerMsg lmsg) [@explicit_arity]) when msg = lmsg -> true
      | _ -> false)

let eventHandler_Register (callbacks : 'msg applicationCallbacks ref)
    (elem : 'a Dom.node_like) (name : string) (handlerType : 'msg eventHandler)
    : 'msg eventCache option =
  let cb = ref (eventHandler_GetCB handlerType) in
  let handler = eventHandler callbacks cb in
  let () = add_event_listener elem name handler in
  (Some { handler; cb } [@explicit_arity])

let eventHandler_Unregister (elem : 'a Dom.node_like) (name : string) :
    'msg eventCache option -> 'msg eventCache option = function
  | None -> None
  | ((Some cache) [@explicit_arity]) ->
      let () = remove_event_listener elem name cache.handler in
      None

let eventHandler_Mutate (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (oldName : string) (newName : string)
    (oldHandlerType : 'msg eventHandler) (newHandlerType : 'msg eventHandler)
    (oldCache : 'msg eventCache option ref)
    (newCache : 'msg eventCache option ref) : unit =
  match !oldCache with
  | None ->
      newCache := eventHandler_Register callbacks elem newName newHandlerType
  | ((Some oldcache) [@explicit_arity]) ->
      if oldName = newName then
        let () = newCache := !oldCache in
        if compareEventHandlerTypes oldHandlerType newHandlerType then ()
        else
          let cb = eventHandler_GetCB newHandlerType in
          let () = oldcache.cb := cb in
          ()
      else
        let () = oldCache := eventHandler_Unregister elem oldName !oldCache in
        let () =
          newCache :=
            eventHandler_Register callbacks elem newName newHandlerType
        in
        ()

let patchVNodesOnElems_PropertiesApply_Add
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
    (_idx : int) : 'msg property -> unit = function
  | NoProp -> ()
  | ((RawProp (k, v)) [@implicit_arity]) -> set_prop elem k v
  | ((Attribute (namespace, k, v)) [@implicit_arity]) ->
      set_attribute' ~namespace elem k v
  | ((Data (k, v)) [@implicit_arity]) ->
      Js.log ("TODO:  Add Data Unhandled", k, v);
      failwith "TODO:  Add Data Unhandled"
  | ((Event (name, handlerType, cache)) [@implicit_arity]) ->
      cache := eventHandler_Register callbacks elem name handlerType
  | ((Style s) [@explicit_arity]) ->
      List.fold_left
        (fun () (k, v) -> set_style_property elem k (Js.Null.return v))
        () s

let patchVNodesOnElems_PropertiesApply_Remove
    (_callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
    (_idx : int) : 'msg property -> unit = function
  | NoProp -> ()
  | ((RawProp (k, _v)) [@implicit_arity]) -> set_prop elem k Js.Undefined.empty
  | ((Attribute (namespace, k, _v)) [@implicit_arity]) ->
      remove_attribute' ~namespace elem k
  | ((Data (k, v)) [@implicit_arity]) ->
      Js.log ("TODO:  Remove Data Unhandled", k, v);
      failwith "TODO:  Remove Data Unhandled"
  | ((Event (name, _, cache)) [@implicit_arity]) ->
      cache := eventHandler_Unregister elem name !cache
  | ((Style s) [@explicit_arity]) ->
      List.fold_left
        (fun () (k, _v) -> set_style_property elem k Js.Null.empty)
        () s

let patchVNodesOnElems_PropertiesApply_RemoveAdd
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (idx : int)
    (oldProp : 'msg property) (newProp : 'msg property) : unit =
  let () =
    patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp
  in
  let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
  ()

let patchVNodesOnElems_PropertiesApply_Mutate
    (_callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
    (_idx : int) (oldProp : 'msg property) : 'msg property -> unit = function
  | NoProp as _newProp ->
      failwith
        "This should never be called as all entries through NoProp are gated."
  | ((RawProp (k, v)) [@implicit_arity]) as _newProp -> set_prop elem k v
  | ((Attribute (namespace, k, v)) [@implicit_arity]) as _newProp ->
      set_attribute' ~namespace elem k v
  | ((Data (k, v)) [@implicit_arity]) as _newProp ->
      Js.log ("TODO:  Mutate Data Unhandled", k, v);
      failwith "TODO:  Mutate Data Unhandled"
  | ((Event (_newName, _newHandlerType, _newCache)) [@implicit_arity]) as
    _newProp ->
      failwith "This will never be called because it is gated"
  | ((Style s) [@explicit_arity]) as _newProp -> (
      match[@ocaml.warning "-4"] oldProp with
      | ((Style oldS) [@explicit_arity]) ->
          List.fold_left2
            (fun () (ok, ov) (nk, nv) ->
              if ok = nk then
                if ov = nv then ()
                else set_style_property elem nk (Js.Null.return nv)
              else
                let () = set_style_property elem ok Js.Null.empty in
                set_style_property elem nk (Js.Null.return nv))
            () oldS s
      | _ ->
          failwith
            "Passed a non-Style to a new Style as a Mutations while the old \
             Style is not actually a style!")

let rec patchVNodesOnElems_PropertiesApply
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (idx : int)
    (oldProperties : 'msg property list) (newProperties : 'msg property list) :
    bool =
  match[@ocaml.warning "-4"] (oldProperties, newProperties) with
  | [], [] -> true
  | [], _newProp :: _newRest -> false
  | _oldProp :: _oldRest, [] -> false
  | NoProp :: oldRest, NoProp :: newRest ->
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (((RawProp (oldK, oldV)) [@implicit_arity]) as oldProp) :: oldRest,
      (((RawProp (newK, newV)) [@implicit_arity]) as newProp) :: newRest ) ->
      let () =
        if oldK = newK && oldV = newV then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (((Attribute (oldNS, oldK, oldV)) [@implicit_arity]) as oldProp) :: oldRest,
      (((Attribute (newNS, newK, newV)) [@implicit_arity]) as newProp)
      :: newRest ) ->
      let () =
        if oldNS = newNS && oldK = newK && oldV = newV then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (((Data (oldK, oldV)) [@implicit_arity]) as oldProp) :: oldRest,
      (((Data (newK, newV)) [@implicit_arity]) as newProp) :: newRest ) ->
      let () =
        if oldK = newK && oldV = newV then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (((Event (oldName, oldHandlerType, oldCache)) [@implicit_arity]) as
       _oldProp)
      :: oldRest,
      (((Event (newName, newHandlerType, newCache)) [@implicit_arity]) as
       _newProp)
      :: newRest ) ->
      let () =
        eventHandler_Mutate callbacks elem oldName newName oldHandlerType
          newHandlerType oldCache newCache
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (((Style oldS) [@explicit_arity]) as oldProp) :: oldRest,
      (((Style newS) [@explicit_arity]) as newProp) :: newRest ) ->
      let () =
        if oldS = newS then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | oldProp :: oldRest, newProp :: newRest ->
      let () =
        patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp
          newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest

let patchVNodesOnElems_Properties (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (oldProperties : 'msg property list)
    (newProperties : 'msg property list) : bool =
  patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties
    newProperties

let genEmptyProps (length : int) : 'msg property list =
  let rec aux lst = function
    | 0 -> lst
    | len -> aux (noProp :: lst) (len - 1)
  in
  aux [] length

let mapEmptyProps (props : 'msg property list) : 'msg property list =
  List.map (fun _ -> noProp) props

let rec patchVNodesOnElems_ReplaceNode
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
    (elems : Dom.element array) (idx : int) : 'msg t -> unit =
  function[@ocaml.warning "-4"]
  | ((Node
       ( newNamespace,
         newTagName,
         _newKey,
         _newUnique,
         newProperties,
         newChildren ))
  [@implicit_arity]) ->
      let oldChild = elems.(idx) in
      let newChild =
        Web.Document.create_element' Web.Document.document
          ~namespace:newNamespace newTagName
      in
      let true =
        patchVNodesOnElems_Properties callbacks newChild
          (mapEmptyProps newProperties)
          newProperties
          [@@ocaml.warning "-8"]
      in
      let childChildren = child_nodes newChild in
      let () =
        patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren
      in
      let _attachedChild = insert_before elem newChild oldChild in
      let _removedChild = remove_child elem oldChild in
      ()
  | _ ->
      failwith
        "Node replacement should never be passed anything but a node itself"

and patchVNodesOnElems_CreateElement (callbacks : 'msg applicationCallbacks ref)
    : 'msg t -> Dom.element = function
  | ((CommentNode s) [@explicit_arity]) ->
      Web.Document.create_comment Web.Document.document s
  | ((Text text) [@explicit_arity]) ->
      Web.Document.create_text_node Web.Document.document text
  | ((Node
       (newNamespace, newTagName, _newKey, _unique, newProperties, newChildren))
  [@implicit_arity]) ->
      let newChild =
        Web.Document.create_element' Web.Document.document
          ~namespace:newNamespace newTagName
      in
      let true =
        patchVNodesOnElems_Properties callbacks newChild
          (mapEmptyProps newProperties)
          newProperties
          [@@ocaml.warning "-8"]
      in
      let childChildren = child_nodes newChild in
      let () =
        patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren
      in
      newChild
  | ((LazyGen (_newKey, newGen, newCache)) [@implicit_arity]) ->
      let vdom = newGen () in
      let () = newCache := vdom in
      patchVNodesOnElems_CreateElement callbacks vdom
  | ((Tagger (tagger, vdom)) [@implicit_arity]) ->
      patchVNodesOnElems_CreateElement (tagger callbacks) vdom

and patchVNodesOnElems_MutateNode (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (elems : Dom.element array) (idx : int)
    (oldNode : 'msg t) (newNode : 'msg t) : unit =
  match (oldNode, newNode) with
  | ( (((Node
          ( _oldNamespace,
            oldTagName,
            _oldKey,
            oldUnique,
            oldProperties,
            oldChildren )) [@implicit_arity]) as _oldNode),
      (((Node
          ( _newNamespace,
            newTagName,
            _newKey,
            newUnique,
            newProperties,
            newChildren )) [@implicit_arity]) as newNode) ) ->
      if oldUnique <> newUnique || oldTagName <> newTagName then
        patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
      else
        let child = elems.(idx) in
        let childChildren = child_nodes child in
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

and patchVNodesOnElems (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (elems : Dom.element array) (idx : int)
    (oldVNodes : 'msg t list) (newVNodes : 'msg t list) : unit =
  match[@ocaml.warning "-4"] (oldVNodes, newVNodes) with
  | ((Tagger (_oldTagger, oldVdom)) [@implicit_arity]) :: oldRest, _ ->
      patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) newVNodes
  | ( oldNode :: oldRest,
      ((Tagger (newTagger, newVdom)) [@implicit_arity]) :: newRest ) ->
      let () =
        patchVNodesOnElems (newTagger callbacks) elem elems idx [ oldNode ]
          [ newVdom ]
      in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | [], [] -> ()
  | [], newNode :: newRest ->
      let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
      let _attachedChild = append_child elem newChild in
      patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
  | _oldVnode :: oldRest, [] ->
      let child = elems.(idx) in
      let _removedChild = remove_child elem child in
      patchVNodesOnElems callbacks elem elems idx oldRest []
  | ( ((CommentNode oldS) [@explicit_arity]) :: oldRest,
      ((CommentNode newS) [@explicit_arity]) :: newRest )
    when oldS = newS ->
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | ( ((Text oldText) [@explicit_arity]) :: oldRest,
      ((Text newText) [@explicit_arity]) :: newRest ) ->
      let () =
        if oldText = newText then ()
        else
          let child = elems.(idx) in
          set_value child newText
      in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | ( ((LazyGen (oldKey, _oldGen, oldCache)) [@implicit_arity]) :: oldRest,
      ((LazyGen (newKey, newGen, newCache)) [@implicit_arity]) :: newRest ) -> (
      if oldKey = newKey then
        let () = newCache := !oldCache in
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else
        match (oldRest, newRest) with
        | ( ((LazyGen (olderKey, _olderGen, _olderCache)) [@implicit_arity])
            :: olderRest,
            ((LazyGen (newerKey, _newerGen, _newerCache)) [@implicit_arity])
            :: newerRest )
          when olderKey = newKey && oldKey = newerKey ->
            let firstChild = elems.(idx) in
            let secondChild = elems.(idx + 1) in
            let _removedChild = remove_child elem secondChild in
            let _attachedChild = insert_before elem secondChild firstChild in
            patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
              newerRest
        | ( ((LazyGen (olderKey, _olderGen, olderCache)) [@implicit_arity])
            :: olderRest,
            _ )
          when olderKey = newKey ->
            let oldChild = elems.(idx) in
            let _removedChild = remove_child elem oldChild in
            let oldVdom = !olderCache in
            let () = newCache := oldVdom in
            patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
        | ( _,
            ((LazyGen (newerKey, _newerGen, _newerCache)) [@implicit_arity])
            :: _newerRest )
          when newerKey = oldKey ->
            let oldChild = elems.(idx) in
            let newVdom = newGen () in
            let () = newCache := newVdom in
            let newChild = patchVNodesOnElems_CreateElement callbacks newVdom in
            let _attachedChild = insert_before elem newChild oldChild in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
        | _ ->
            let oldVdom = !oldCache in
            let newVdom = newGen () in
            let () = newCache := newVdom in
            patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest)
              (newVdom :: newRest))
  | ( (((Node
          ( oldNamespace,
            oldTagName,
            oldKey,
            _oldUnique,
            _oldProperties,
            _oldChildren )) [@implicit_arity]) as oldNode)
      :: oldRest,
      (((Node
          ( newNamespace,
            newTagName,
            newKey,
            _newUnique,
            _newProperties,
            _newChildren )) [@implicit_arity]) as newNode)
      :: newRest ) -> (
      if oldKey = newKey && oldKey <> "" then
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else if oldKey = "" || newKey = "" then
        let () =
          patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode
        in
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else
        match (oldRest, newRest) with
        | ( ((Node
               ( olderNamespace,
                 olderTagName,
                 olderKey,
                 _olderUnique,
                 _olderProperties,
                 _olderChildren )) [@implicit_arity])
            :: olderRest,
            ((Node
               ( newerNamespace,
                 newerTagName,
                 newerKey,
                 _newerUnique,
                 _newerProperties,
                 _newerChildren )) [@implicit_arity])
            :: newerRest )
          when olderNamespace = newNamespace
               && olderTagName = newTagName && olderKey = newKey
               && oldNamespace = newerNamespace
               && oldTagName = newerTagName && oldKey = newerKey ->
            let firstChild = elems.(idx) in
            let secondChild = elems.(idx + 1) in
            let _removedChild = remove_child elem secondChild in
            let _attachedChild = insert_before elem secondChild firstChild in
            patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
              newerRest
        | ( ((Node
               ( olderNamespace,
                 olderTagName,
                 olderKey,
                 _olderUnique,
                 _olderProperties,
                 _olderChildren )) [@implicit_arity])
            :: olderRest,
            _ )
          when olderNamespace = newNamespace
               && olderTagName = newTagName && olderKey = newKey ->
            let oldChild = elems.(idx) in
            let _removedChild = remove_child elem oldChild in
            patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
        | ( _,
            ((Node
               ( newerNamespace,
                 newerTagName,
                 newerKey,
                 _newerUnique,
                 _newerProperties,
                 _newerChildren )) [@implicit_arity])
            :: _newerRest )
          when oldNamespace = newerNamespace
               && oldTagName = newerTagName && oldKey = newerKey ->
            let oldChild = elems.(idx) in
            let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
            let _attachedChild = insert_before elem newChild oldChild in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
        | _ ->
            let () =
              patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode
                newNode
            in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest)
  | _oldVnode :: oldRest, newNode :: newRest ->
      let oldChild = elems.(idx) in
      let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
      let _attachedChild = insert_before elem newChild oldChild in
      let _removedChild = remove_child elem oldChild in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest

let patchVNodesIntoElement (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (oldVNodes : 'msg t list) (newVNodes : 'msg t list) :
    'msg t list =
  let elems = child_nodes elem in
  let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in
  newVNodes

let patchVNodeIntoElement (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (oldVNode : 'msg t) (newVNode : 'msg t) : 'msg t list =
  patchVNodesIntoElement callbacks elem [ oldVNode ] [ newVNode ]

let wrapCallbacks_On : type a b. (a -> b) -> a systemMessage -> b systemMessage
    =
 fun func -> function
  | Render -> Render
  | ((AddRenderMsg msg) [@explicit_arity]) ->
      AddRenderMsg (func msg) [@explicit_arity]
  | ((RemoveRenderMsg msg) [@explicit_arity]) ->
      RemoveRenderMsg (func msg) [@explicit_arity]

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
      (Tagger (Obj.magic tagger, Obj.magic vdom) [@implicit_arity])
     : ('a -> 'b) -> 'a t -> 'b t)
    : ('a -> 'b) -> 'a t -> 'b t)
