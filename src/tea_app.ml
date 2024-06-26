open Web_node

type ('msg, 'model) cmd_update = 'model -> 'msg -> 'model * 'msg Tea_cmd.t
type ('msg, 'model) update = 'model -> 'msg -> 'model
type ('msg, 'model) view = 'model -> 'msg Vdom.Node.t

type ('flags, 'model, 'msg) program = {
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : ('msg, 'model) cmd_update;
  view : ('msg, 'model) view;
  subscriptions : 'model -> 'msg Tea_sub.t;
  shutdown : 'model -> 'msg Tea_cmd.t;
}

type ('flags, 'model, 'msg) standardProgram = {
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : ('msg, 'model) cmd_update;
  view : ('msg, 'model) view;
  subscriptions : 'model -> 'msg Tea_sub.t;
}

type ('model, 'msg) beginnerProgram = {
  model : 'model;
  update : ('msg, 'model) update;
  view : ('msg, 'model) view;
}

type ('model, 'msg) pumpInterface = {
  startup : unit -> unit;
  render_string : 'model -> string;
  handleMsg : 'model -> 'msg -> 'model;
  shutdown : 'msg Tea_cmd.t -> unit;
}

module ProgramInterface = struct
  type 'msg t =
    < pushMsg : 'msg -> unit
    ; shutdown : unit -> unit
    ; getHtmlString : unit -> string >
    Js.t

  external make :
    pushMsg:('msg -> unit) ->
    shutdown:(unit -> unit) ->
    getHtmlString:(unit -> string) ->
    'msg t = ""
  [@@mel.obj]
end

let programStateWrapper initModel pump shutdown =
  let open Vdom.ApplicationCallbacks in
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
        pending := Some [];
        let newModel = pumperInterface.handleMsg !model msg in
        model := newModel;
        match !pending with
        | None ->
            failwith
              "INVALID message queue state, should never be None during \
               message processing!"
        | Some [] -> pending := None
        | Some msgs ->
            pending := None;
            List.iter handler (List.rev msgs))
    | Some msgs -> pending := Some (msg :: msgs)
  in
  let render_events = ref [] in
  let finalizedCBs =
    (({
        enqueue = (fun msg -> handler msg);
        on =
          (function
          | Render -> List.iter handler !render_events
          | AddRenderMsg msg ->
              render_events := List.append !render_events [ msg ]
          | RemoveRenderMsg msg ->
              render_events := List.filter (fun mg -> msg != mg) !render_events);
      }
       : 'msg Vdom.ApplicationCallbacks.t)
      : 'msg Vdom.ApplicationCallbacks.t)
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

  ProgramInterface.make ~pushMsg:handler ~shutdown:pi_requestShutdown
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
          render_string = (fun model -> Vdom.Node.to_string (view model));
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
                Vdom.Node.patch_nodes_into_element callbacks parentNode
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
              (*   nextFrameID := (Some (-1) ); *)
              (*   doRender 16) *)
              (* else *)
              let id = Web.Window.request_animation_frame doRender in
              nextFrameID := Some id
        in

        let clearPnode () =
          while Js.Array.length (child_nodes parentNode) > 0 do
            first_child parentNode
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
          nextFrameID := Some (-1);
          doRender 16
        in

        let render_string model = Vdom.Node.to_string (view model) in

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
       'msg ProgramInterface.t)
    : ('flags, 'model, 'msg) program ->
      'a Dom.node_like Js.Nullable.t ->
      'flags ->
      'msg ProgramInterface.t)

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
       'msg ProgramInterface.t)
    : ('flags, 'model, 'msg) standardProgram ->
      'a Dom.node_like Js.Nullable.t ->
      'flags ->
      'msg ProgramInterface.t)

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
       'msg ProgramInterface.t)
    : ('model, 'msg) beginnerProgram ->
      'a Dom.node_like Js.Nullable.t ->
      unit ->
      'msg ProgramInterface.t)

let map func vnode = Vdom.map func vnode
