(* hide *)
module Demos.MovingBox
(* end-hide *)

(**
### Elmish component

From here this is a standard Elmish component
*)
module Demo =

    open Elmish
    open Fable.PowerPack
    open Fable.Import
    open Fable.Core.JsInterop

    (**
#### Model
    *)

    type Model =
        { CanvasWidth : float
          CanvasHeight : float
          Box : Position
          BoxVelocity : float }

    (**
#### Msg
    *)
    type Msg =
        /// The tick message is trigger at 60fps, and is responsible for the animation trigger
        | Tick of float * float
        (* hide *)
        | UpdateCanvasSize of float * float
        (* end-hide *)

    (**
#### Init function
    *)
    let init () =
        { CanvasWidth = 0.
          CanvasHeight = 0.
          Box =
            { X = 20.
              Y = 20. }
          BoxVelocity = 0.08 }, Cmd.none

    (**
#### Update function
    *)
    let update msg model =
        match msg with
        // Update the animation
        | Tick (delta, _) ->
            let boxPos = model.Box.X + model.BoxVelocity * delta
            let boxVelocity =
                if boxPos <= 0. || boxPos >= model.CanvasWidth / 4. then
                    -model.BoxVelocity
                else
                    model.BoxVelocity

            { model with Box =
                            { model.Box with X = boxPos }
                         BoxVelocity = boxVelocity }, Cmd.none

        (* hide *)
        | UpdateCanvasSize (width, height) ->
            { model with CanvasWidth = width
                         CanvasHeight = height }, Cmd.none
        (* end-hide *)

    (**
#### Views
    *)
    let drawBox (model : Model) =
        [ Canvas.FillStyle !^"firebrick"
          Canvas.FillRect (model.Box.X, model.Box.Y, 40., 40.) ]
        |> Canvas.Batch

    let view (model : Model) width dispatch =
        let size : Canvas.Size =
            { Width = model.CanvasWidth
              Height = model.CanvasHeight }

        size
        |> Canvas.initialize
        |> Canvas.onTick (Tick >> dispatch)
        |> Canvas.withMaxFPS 10
        |> Canvas.draw (Canvas.ClearRect (0., 0., model.CanvasWidth, model.CanvasHeight))
        |> Canvas.draw (drawBox model)
        |> Canvas.render

(* hide *)
/////////////////////////////////////////////////////////////////////
/// FROM HERE THE CODE IS DEDICATED TO THE HTML PART OF THE VIEW
/// - Settings
/// - layout
/// - etc.
////////////////////////////////////////////////////////////////////
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Thoth.Elmish
open System

type Inputs =
    { Size : string
      FollowSpeed : string
      NumOfSegments : string }

type Model =
    { Demo : Demo.Model
      Inputs : Inputs
      Debouncer : Debouncer.State }

type Msg =
    | OnResize of (float * float)
    | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
    | DemoMsg of Demo.Msg

let init _ =
    let (demoModel, demoCmd) = Demo.init ()

    { Inputs =
        { Size = "25"
          NumOfSegments = "16"
          FollowSpeed = "0.1" }
      Debouncer = Debouncer.create ()
      Demo = demoModel  }, Cmd.map DemoMsg demoCmd

let update msg model =
    match msg with
    | OnResize size ->
        model, Demo.Msg.UpdateCanvasSize size
                |> DemoMsg
                |>Cmd.ofMsg

    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg model.Debouncer
        { model with Debouncer = debouncerModel }, debouncerCmd

    | DemoMsg demoMsg ->
        let (demoModel, demoCmd) = Demo.update demoMsg model.Demo
        { model with Demo = demoModel }, Cmd.map DemoMsg demoCmd

let view (model : Model) dispatch =
    div [ Style [ Margin "1em" ] ]
        [ Content.content [ ]
            [ Heading.h4 [ ]
                [ str "Moving box" ]
              Heading.p [ Heading.Is6
                          Heading.IsSubtitle ]
                [ str "This project has been ported from "
                  a [ Href "https://codepen.io/anon/pen/PBrVdO"
                      Class "is-italic" ]
                    [ str "this codepen" ] ] ]
          div [ Class "demo" ]
            [ div [ ]
                [ ReactResizeDetector.detector [ ReactResizeDetector.HandleHeight
                                                 ReactResizeDetector.HandleWidth
                                                 ReactResizeDetector.OnResize (fun width height ->
                                                    dispatch (OnResize (width, height))
                                                ) ] [ ]
                  Demo.view model.Demo (float model.Inputs.Size) (DemoMsg >> dispatch) ] ]
          div [ Style [ MaxWidth "800px"
                        Margin "auto" ] ]
            [ Elmish.React.Common.lazyView Helpers.View.literateCode (__SOURCE_DIRECTORY__ + "/" + __SOURCE_FILE__) ] ]
