# Melange-TEA

## Description

Melange-TEA is an OCaml library that allows the creation of dynamic web frontends using Melange.

For now I am attempting to follow TEA/[The Elm Architecture](https://guide.elm-lang.org/architecture/) as a base (as this project is a fork of [bucklescript-tea](https://github.com/OvermindDL1/bucklescript-tea). However, I will likely make changes to better fit the OCaml ecosystem.

## Goals of the project

- Make the API close enough to Elm to be able to port code easily
- Play to the strengths of OCaml to add features Elm could not easily provide
- Provide a language-agnostic (I.E. OCaml vs Reason) solution for creating frontend web applications

## Bucklescript-TEA's planned features that are still on the table

- [X] Elm API: Following the Elm API as closely as OCaml allows.
- [ ] OCamlized-TEA: The Elm API is succinct, but highly inefficient in the amount of allocations it causes, though this is not necessary it would be nice to have a replacement API that takes effort to reduce the amount of allocations. Most real-world use would get near nothing out of this but for a few cases it would be quite useful to have an overhaul of the Virtual-DOM declaration style.
- [ ] React: It would also be nice to have a React back-end for easier integration with React projects, both into and out of this component. This should not have any breaking change over the Elm API but would just be an extension on it.

## Changes

### Already Done

- Usage of `melange.dom` types and `[@@mel.send]` vs bucklescript-tea's method-based approach
    - Doesn't rely on not-currently-documented features from Melange
    - Fits more with the functional style I'm going for
- Removed `Tea_result` as we are far past the OCaml version where `result` was added with Melange
- Removed the Reason.ml code since it was generated anyway and this library should work with both OCaml and Reason syntax without it
- Changed the `test` code to use Vite for rendering

### Proposed

- Copy Elm's `Browser` module structure
- Remove the XMLHttpRequest-related code or at least port it to fetch (either custom or using `melange-fetch`)
- Replace Tea.Html with Tea.Html2's code. Fully embrace the current Elm architecture for now before making other changes.
    - This will break some people's older projects if they update to melange + melange-tea
- Add Style and Attribute constructors (An example would be the `feliz` project for Fable)
    - Removes the need for so many string literals everywhere (fewer chances of misspelling mistakes and such)
- Allow optional interop/wrapping of React components
    - If there's a way to do it I'd also like Preact and/or WebComponents as well
- Refactor, refactor, refactor

## Usage

This project is currently not up on opam. If you want to run the examples in `test` you can clone the repo and do the following:

Install dependencies with

```bash
npm i
npm run init
```

Then run the project using

```bash
npm run dev
```

To change which test you are looking at, you can change the module being called in `test/test_client.ml`

### Example project

```ocaml
(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html

(* Let's create a new type here to be our main message type that is passed around *)
type msg =
  | Increment  (* This will be our message to increment the counter *)
  | Decrement  (* This will be our message to decrement the counter *)
  | Reset      (* This will be our message to reset the counter to 0 *)
  | Set of int (* This will be our message to set the counter to a specific value *)

(* This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values, the model for Counter is just an integer *)
let init () = 4

(* This is the central message handler, it takes the model as the first argument *)
let update model = function (* These should be simple enough to be self-explanatory, mutate the model based on the message, easy to read and follow *)
  | Increment -> model + 1
  | Decrement -> model - 1
  | Reset -> 0
  | Set v -> v

(* This is just a helper function for the view, a simple function that returns a button based on some argument *)
let view_button title msg =
  button
    [ Events.onClick msg
    ]
    [ text title
    ]

(* This is the main callback to generate the virtual-dom.
  This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing *)
let view model =
  div
    []
    [ span
        [ Attributes.style "text-weight" "bold" ]
        [ Attributes.text (string_of_int model) ]
    ; br []
    ; view_button "Increment" Increment
    ; br []
    ; view_button "Decrement" Decrement
    ; br []
    ; view_button "Set to 42" (Set 42)
    ; br []
    ; if model <> 0 then view_button "Reset" Reset else noNode
    ]

(* This is the main function, it can be named anything you want but `main` is traditional.
  The Program returned here has a set of callbacks that can easily be called from
  melange or from javascript for running this main attached to an element,
  or even to pass a message into the event loop. *)
let main =
  beginnerProgram { (* The beginnerProgram just takes a set model state and the update and view functions *)
    model = init (); (* Since model is a set value here, we call our init function to generate that value *)
    update;
    view;
  }
```

If anything is typed wrong than the OCaml type checker will catch it and advise. Compilation times are wonderfully fast, probably faster than about any other compile-to-javascript language that you will come across.

```javascript
  var app = require("src/counter.ml").main(document.getElementById("my-element"));
```
