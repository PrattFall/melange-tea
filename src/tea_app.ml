open Web_node

type ('flags, 'model, 'msg) program = {
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.t;
  subscriptions : 'model -> 'msg Tea_sub.t;
  shutdown : 'model -> 'msg Tea_cmd.t;
}

type ('flags, 'model, 'msg) standardProgram = {
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.t;
  subscriptions : 'model -> 'msg Tea_sub.t;
}

type ('model, 'msg) beginnerProgram = {
  model : 'model;
  update : 'model -> 'msg -> 'model;
  view : 'model -> 'msg Vdom.t;
}

type ('model, 'msg) pumpInterface = {
  startup : unit -> unit;
  render_string : 'model -> string;
  handleMsg : 'model -> 'msg -> 'model;
  shutdown : 'msg Tea_cmd.t -> unit;
}

type 'msg programInterface =
  < pushMsg : 'msg -> unit
  ; shutdown : unit -> unit
  ; getHtmlString : unit -> string >
  Js.t

external makeProgramInterface :
  pushMsg:('msg -> unit) ->
  shutdown:(unit -> unit) ->
  getHtmlString:(unit -> string) ->
  'msg programInterface = ""
[@@mel.obj]

let programStateWrapper initModel pump shutdown =
  let open Vdom in
  let model = ref initModel in
  let callbacks =
    ref
      {
        enqueue = (fun _msg -> Js.log "INVALID enqueue CALL!");
        on = (fun _ -> ());
      }
  in
  let pumperInterfaceC () = pump callbacks in
  let pumperInterface = pumperInterfaceC () in
  let pending = ((ref None : 'msg list option ref) : 'msg list option ref) in
  let rec handler msg =
    match !pending with
    | None -> (
        pending := (Some [] [@explicit_arity]);
        let newModel = pumperInterface.handleMsg !model msg in
        model := newModel;
        match !pending with
        | None ->
            failwith
              "INVALID message queue state, should never be None during \
               message processing!"
        | ((Some []) [@explicit_arity]) -> pending := None
        | ((Some msgs) [@explicit_arity]) ->
            pending := None;
            List.iter handler (List.rev msgs))
    | ((Some msgs) [@explicit_arity]) ->
        pending := (Some (msg :: msgs) [@explicit_arity])
  in
  let render_events = ref [] in
  let finalizedCBs =
    (({
        enqueue = (fun msg -> handler msg);
        on =
          (function
          | Render -> List.iter handler !render_events
          | ((AddRenderMsg msg) [@explicit_arity]) ->
              render_events := List.append !render_events [ msg ]
          | ((RemoveRenderMsg msg) [@explicit_arity]) ->
              render_events := List.filter (fun mg -> msg != mg) !render_events);
      }
       : 'msg Vdom.applicationCallbacks)
      : 'msg Vdom.applicationCallbacks)
  in
  callbacks := finalizedCBs;

  let pi_requestShutdown () =
    callbacks :=
      {
        enqueue = (fun _msg -> Js.log "INVALID message enqueued when shut down");
        on = (fun _ -> ());
      };
    pumperInterface.shutdown (shutdown !model)
  in

  let render_string () = pumperInterface.render_string !model in

  pumperInterface.startup ();

  makeProgramInterface ~pushMsg:handler ~shutdown:pi_requestShutdown
    ~getHtmlString:render_string

let programLoop update view subscriptions initModel initCmd = function
  | None ->
      fun callbacks ->
        let oldSub = ref Tea_sub.none in
        let handleSubscriptionChange model =
          let newSub = subscriptions model in
          oldSub := Tea_sub.run callbacks callbacks !oldSub newSub
        in
        {
          startup =
            (fun () ->
              Tea_cmd.run callbacks initCmd;
              handleSubscriptionChange initModel);
          render_string =
            (fun model ->
              let vdom = view model in
              let rendered = Vdom.renderToHtmlString vdom in
              rendered);
          handleMsg =
            (fun model msg ->
              let newModel, cmd = update model msg in
              Tea_cmd.run callbacks cmd;
              handleSubscriptionChange newModel;
              newModel);
          shutdown =
            (fun cmd ->
              Tea_cmd.run callbacks cmd;
              oldSub := Tea_sub.run callbacks callbacks !oldSub Tea_sub.none);
        }
  | Some parentNode ->
      fun callbacks ->
        let priorRenderedVdom = ref [] in
        let latestModel = ref initModel in
        let nextFrameID = ref None in

        let doRender _delta =
          Option.iter
            (fun _id ->
              let newVdom = [ view !latestModel ] in
              let justRenderedVdom =
                Vdom.patchVNodesIntoElement callbacks parentNode
                  !priorRenderedVdom newVdom
              in
              priorRenderedVdom := justRenderedVdom;
              !callbacks.on Render;
              nextFrameID := None)
            !nextFrameID
        in

        let scheduleRender () =
          match !nextFrameID with
          | Some _ -> ()
          | None ->
              (* (* context needed for this *) *)
              (* let realtimeRendering = false in *)
              (* if realtimeRendering then ( *)
              (*   nextFrameID := (Some (-1) [@explicit_arity]); *)
              (*   doRender 16) *)
              (* else *)
              let id = Web.Window.request_animation_frame doRender in
              nextFrameID := Some id
        in

        let clearPnode () =
          while Js.Array.length (child_nodes parentNode) > 0 do
            Js.Nullable.toOption (first_child parentNode)
            |> Option.iter (fun child -> ignore (remove_child parentNode child))
          done
        in

        let oldSub = ref Tea_sub.none in

        let handleSubscriptionChange model =
          let newSub = subscriptions model in
          oldSub := Tea_sub.run callbacks callbacks !oldSub newSub
        in

        let handlerStartup () =
          clearPnode ();
          Tea_cmd.run callbacks initCmd;
          handleSubscriptionChange !latestModel;
          nextFrameID := (Some (-1) [@explicit_arity]);
          doRender 16
        in

        let render_string model = Vdom.renderToHtmlString (view model) in

        let handler model msg =
          let newModel, cmd = update model msg in
          latestModel := newModel;
          Tea_cmd.run callbacks cmd;
          scheduleRender ();
          handleSubscriptionChange newModel;
          newModel
        in

        let handlerShutdown cmd =
          nextFrameID := None;
          Tea_cmd.run callbacks cmd;
          oldSub := Tea_sub.run callbacks callbacks !oldSub Tea_sub.none;
          priorRenderedVdom := [];
          clearPnode ()
        in

        {
          startup = handlerStartup;
          render_string;
          handleMsg = handler;
          shutdown = handlerShutdown;
        }

let program =
  ((fun { init; update; view; subscriptions; shutdown } pnode flags ->
      Web.polyfills ();
      let initModel, initCmd = init flags in
      let opnode = Js.Nullable.toOption pnode in
      let pumpInterface =
        programLoop update view subscriptions initModel initCmd opnode
      in
      programStateWrapper initModel pumpInterface shutdown
     : ('flags, 'model, 'msg) program ->
       'a Dom.node_like Js.Nullable.t ->
       'flags ->
       'msg programInterface)
    : ('flags, 'model, 'msg) program ->
      'a Dom.node_like Js.Nullable.t ->
      'flags ->
      'msg programInterface)

let standardProgram =
  ((fun { init; update; view; subscriptions } pnode args ->
      program
        {
          init;
          update;
          view;
          subscriptions;
          shutdown = (fun _model -> Tea_cmd.none);
        }
        pnode args
     : ('flags, 'model, 'msg) standardProgram ->
       'a Dom.node_like Js.Nullable.t ->
       'flags ->
       'msg programInterface)
    : ('flags, 'model, 'msg) standardProgram ->
      'a Dom.node_like Js.Nullable.t ->
      'flags ->
      'msg programInterface)

let beginnerProgram =
  ((fun { model; update; view } pnode () ->
      standardProgram
        {
          init = (fun () -> (model, Tea_cmd.none));
          update = (fun model msg -> (update model msg, Tea_cmd.none));
          view;
          subscriptions = (fun _model -> Tea_sub.none);
        }
        pnode ()
     : ('model, 'msg) beginnerProgram ->
       'a Dom.node_like Js.Nullable.t ->
       unit ->
       'msg programInterface)
    : ('model, 'msg) beginnerProgram ->
      'a Dom.node_like Js.Nullable.t ->
      unit ->
      'msg programInterface)

let map func vnode = Vdom.map func vnode
