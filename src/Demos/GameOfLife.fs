(* hide *)
module Demos.GameOfLife
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
    open Fulma
    open Fulma.FontAwesome

    (**
#### Model
    *)

    type Model =
        { CanvasWidth : float
          CanvasHeight : float
          World : array<array<int>>
          MaxFPS : float
          LastFrameTimeMs : float
          CellSize : float
          IterationRank : int
          IsRunning : bool }

    (**
#### Msg
    *)
    type Msg =
        /// The tick message is trigger at 60fps, and is responsible for the animation trigger
        | Tick of float
        (* hide *)
        | UpdateCanvasSize of float * float
        | MouseDown of Position
        | ToggleState
        (* end-hide *)

    let private rand = new System.Random()

    (**
#### Init function
    *)
    let private emptyWorld _ =
        Array.init 200
            (fun _ -> Array.init 200 (fun _ ->
                rand.NextDouble() * 10. |> int
            ))

    let init () =
        { CanvasWidth = 0.
          CanvasHeight = 0.
          World = emptyWorld ()
          MaxFPS = 10.
          LastFrameTimeMs = 0.
          CellSize = 25.
          IterationRank = 0
          IsRunning = false }, Cmd.none

    (**
#### Update function
    *)
    let [<Literal>] LIVE = 1
    let [<Literal>] DEAD = 0

    let calculateNeighbourgs (x : int) (y : int) (world : array<array<int>>) =
        let ySize = world.Length
        let xSize = world.[0].Length

        [ -1,-1 ; 0,-1 ; 1,-1;
          -1, 0 ;        1, 0;
          -1, 1 ; 0, 1 ; 1, 1; ]
        |> List.map (fun (dx, dy) ->
            let x = x + dx
            let y = y + dy
            if x >= 0
                && x < xSize
                && y >= 0
                && y < ySize
                && world.[y].[x] = 1 then
                    1
            else
                0
        )
        |> List.sum

    let mutable canvasRef : Browser.HTMLCanvasElement = null

    module Cmd =

        let onAnimationFrame (messageCtor : float -> 'Msg) =
            let handler dispatch =
                Browser.window.requestAnimationFrame(fun timestamp ->
                    messageCtor timestamp |> dispatch
                ) |> ignore
            [ handler ]

    let update msg model =
        match msg with
        // Update the animation
        | Tick timestamp ->
            if model.IsRunning then
                if timestamp < model.LastFrameTimeMs + (1000. / model.MaxFPS) then
                    // printfn "tick part 1"
                    model, Cmd.onAnimationFrame Tick
                else

                    let ySize = model.World.Length
                    let xSize = model.World.[0].Length

                    let newWorld =
                        [| for y = 0 to ySize - 1 do
                            yield
                                [| for x = 0 to xSize - 1 do
                                    let cell = model.World.[y].[x]
                                    if cell = LIVE then
                                        // A live cell, stay a live cell if it has 2 or 3 neighbougs
                                        match calculateNeighbourgs x y model.World with
                                        | 2
                                        | 3 -> yield LIVE
                                        | _ -> yield DEAD
                                    else
                                        // A dead cell, becomes a live cell if it has 3 neighbourgs
                                        match calculateNeighbourgs x y model.World with
                                        | 3 -> yield LIVE
                                        | _ -> yield DEAD |]
                        |]
                    // printfn "tick part 2"
                    { model with World = newWorld
                                 LastFrameTimeMs = timestamp } , Cmd.onAnimationFrame Tick
            else
                model, Cmd.none

        | ToggleState ->
            { model with IsRunning = not model.IsRunning }, Cmd.onAnimationFrame Tick

        | MouseDown position ->
            let x = JS.Math.floor (position.X / model.CellSize) |> int
            let y = JS.Math.floor (position.Y / model.CellSize) |> int

            let newWorld =
                model.World
                |> Array.mapi (fun index column ->
                    if index = y then
                        column
                        |> Array.mapi (fun index cell ->
                            if index = x then
                                // Inverse cell's state
                                if cell = LIVE then
                                    DEAD
                                else
                                    LIVE
                            else
                                cell
                        )
                    else
                        column
                )

            { model with World = newWorld }, Cmd.none

        (* hide *)
        | UpdateCanvasSize (width, height) ->
            { model with CanvasWidth = width
                         CanvasHeight = height }, Cmd.none
        (* end-hide *)

    (**
#### Views
    *)
    let drawCell (x : int) (y : int) (cellSize : float) =
        [ Canvas.FillStyle !^"green"
          Canvas.FillRect (float x * cellSize , float y * cellSize, cellSize, cellSize) ]
        |> Canvas.Batch

    let drawWorld (model : Model) =
        let ySize = model.World.Length
        let xSize = model.World.[0].Length

        let cells =
            [ for y = 0 to ySize - 1 do
                for x = 0 to xSize - 1 do
                    if model.World.[y].[x] = 1 then
                        yield drawCell x y model.CellSize
            ]
            |> Canvas.Batch

        let grid =
            let mutable x = model.CellSize
            let mutable y = model.CellSize

            [ while x < canvasRef.width do
                yield Canvas.BeginPath
                yield Canvas.MoveTo (x, 0.)
                yield Canvas.LineTo (x, canvasRef.height)
                yield Canvas.Stroke

                x <- x + model.CellSize

              while y < canvasRef.height do
                yield Canvas.BeginPath
                yield Canvas.MoveTo (0., y)
                yield Canvas.LineTo (canvasRef.width, y)
                yield Canvas.Stroke

                y <- y + model.CellSize
             ]
            |> Canvas.Batch

        [ cells
          Canvas.StrokeStyle !^"black"
          grid
        ] |> Canvas.Batch

    let private renderCanvas (model : Model) width dispatch =
        [ Canvas.ClearRect (0., 0., model.CanvasWidth, model.CanvasHeight)
          Canvas.FillStyle !^"lightgrey"
          Canvas.FillRect (0., 0., model.CanvasWidth, model.CanvasHeight)
          drawWorld model ]

    open Fable.Helpers.React
    open Fable.Helpers.React.Props

    let private canvasArea model dispatch =
        if not (isNull canvasRef) then
            let context = canvasRef.getContext_2d()
            Canvas.drawOps context (renderCanvas model 0. dispatch)

        div [ ]
            [ ReactResizeDetector.detector [ ReactResizeDetector.HandleHeight
                                             ReactResizeDetector.HandleWidth
                                             ReactResizeDetector.OnResize (fun width height ->
                                                dispatch (UpdateCanvasSize (width, height))
                                            ) ] [ ]
              canvas [ HTMLAttr.Width model.CanvasWidth
                       HTMLAttr.Height model.CanvasHeight
                       OnMouseDown (fun ev ->
                            let bounds : Browser.ClientRect = !!ev.target?getBoundingClientRect()
                            { X = ev.clientX - bounds.left
                              Y = ev.clientY - bounds.top }
                            |> MouseDown
                            |> dispatch
                         )
                       Ref (fun elt ->
                            if not (isNull elt) && isNull canvasRef then
                                canvasRef <- elt :?> Browser.HTMLCanvasElement
                                dispatch (Tick 0.)
                         ) ]
                    [ ] ]
    let view (model : Model) width dispatch =
        let icon =
            if model.IsRunning then
                Fa.icon Fa.I.Pause
            else
                Fa.icon Fa.I.Play

        div [ ]
            [ Level.level [ ]
                [ Level.item [ ]
                    [ Button.button [ Button.IsOutlined
                                      Button.OnClick (fun _ ->
                                        dispatch ToggleState
                                      ) ]
                        [ Icon.faIcon [ ]
                            [ icon ] ]
                    ]
                ]
              canvasArea model dispatch ]

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
      Demo = demoModel }, Cmd.map DemoMsg demoCmd

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

let settings model dispatch =
    div [ Class "settings-row" ]
        [ Field.div [ ]
            [ Label.label [ ]
                [ str "Number of segments"
                  Control.div [ ]
                    [ Input.input [ // Input.OnChange (fun ev ->
                                                    // ev.Value
                                                    // |> ChangeNum
                                                    // |> dispatch)
                                    Input.Value (string model.NumOfSegments)

                                    Input.Props [ Min "5"
                                                  Max "30"
                                                  Step "1"
                                                  HTMLAttr.Type "range" ] ] ] ] ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Follow speed"
                  Control.div [ ]
                    [ Input.number [ // Input.OnChange (fun ev ->
                                                    // ev.Value
                                                    // |> ChangeFollowSpeed
                                                    // |> dispatch)
                                     Input.Value (string model.FollowSpeed)
                                     Input.Props [ Min "0.05"
                                                   Max "0.25"
                                                   Step "0.01"
                                                   HTMLAttr.Type "range" ] ] ] ] ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Size"
                  Control.div [ ]
                    [ Input.number [ // Input.OnChange (fun ev ->
                                                    // ev.Value
                                                    // |> ChangeSize
                                                    // |> dispatch)
                                     Input.Value (string model.Size)
                                     Input.Props [ Min "5"
                                                   Max "40"
                                                   Step "2"
                                                   HTMLAttr.Type "range" ] ] ] ] ] ]

let view (model : Model) dispatch =
    div [ Style [ Margin "1em" ] ]
        [ Content.content [ ]
            [ Heading.h4 [ ]
                [ str "Game of Life" ]
              Heading.p [ Heading.Is6
                          Heading.IsSubtitle ]
                [ str "This project has been ported from "
                  a [ Href "https://codepen.io/anon/pen/PBrVdO"
                      Class "is-italic" ]
                    [ str "this codepen" ] ] ]
          div [ Class "demo" ]
            [ settings model.Inputs dispatch
              Demo.view model.Demo (float model.Inputs.Size) (DemoMsg >> dispatch) ]
          div [ Style [ MaxWidth "800px"
                        Margin "auto" ] ]
            [ Elmish.React.Common.lazyView Helpers.View.literateCode (__SOURCE_DIRECTORY__ + "/" + __SOURCE_FILE__) ] ]
