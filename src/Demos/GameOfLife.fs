(* hide *)
module Demos.GameOfLife
(* end-hide *)

module GlobalListener =

    open Fable.Core
    open Fable.Import
    open Fable.Helpers.React
    open Fable.Helpers.React.Props

    [<Pojo>]
    type Props =
        { OnMount : unit -> unit
          OnUnmount: unit -> unit }

    [<Pojo>]
    type State = obj

    type GlobalListener(props) =
        inherit React.Component<Props, State>(props)
        do base.setInitState(null)

        override __.componentWillUnmount () =
            props.OnUnmount ()

        override __.componentDidMount () =
            props.OnMount ()

        override this.render () =
            fragment [ ]
                [ ]

    let view props =
        ofType<GlobalListener,_,_> props [ ]

(**
### Elmish component
*)
module Demo =

    open Elmish
    open Fable.Import
    open Fable.Core.JsInterop
    open Fulma
    open Fulma.FontAwesome

    (**
#### Model
    *)
    module Const =
        let [<Literal>] GRID_SIZE = 30
        let [<Literal>] MAX_FPS = 10.
        let [<Literal>] ALIVE = 1
        let [<Literal>] DEAD = 0

    /// Type alias to describe a cell in our World
    ///
    /// A cell can have two state:
    /// - Alive: 1
    /// - Dead: 0
    type Cell = int

    type World = array<array<Cell>>

    type Model =
        { /// Width of the canvas
          CanvasWidth : float
          /// Height of the canvas
          CanvasHeight : float
          /// Current world state.
          World : World
          /// Last timestamp received from a RAF call
          ///
          /// We use it in order to calculate the current FPS
          LastFrameTimeMs : float
          /// Width of a cell. It's calculated from the current size of the canvas
          CellWidth : float
          /// Number of iteration in the Game of Life since the start
          IterationRank : int
          /// Is the game running
          IsRunning : bool
          /// Is the user dragging
          IsDragging : bool }

    (**
#### Msg
    *)
    type Msg =
        /// The tick message is trigger at 60fps, and is responsible for the animation trigger
        | Tick of float
        (* hide *)
        /// Trigger when the canvas size is updated.
        ///
        /// Ex:
        /// - First load of the element
        /// - Resize of the window
        | UpdateCanvasSize of float * float
        | MouseDown of Position
        | MouseUp
        | MouseDrag of Position
        /// Allow to toggle between Play/Pause
        | ToggleState
        (* end-hide *)

    (**
#### Init function
    *)


    let private emptyWorld _ =
        let rand = new System.Random()

        Array.init Const.GRID_SIZE
            (fun _ -> Array.init Const.GRID_SIZE (fun _ ->
                rand.NextDouble() * 10. |> int
            ))

    let init () =
        { CanvasWidth = 0.
          CanvasHeight = 0.
          World = emptyWorld ()
          LastFrameTimeMs = 0.
          CellWidth = 0.
          IterationRank = 0
          IsRunning = false
          IsDragging = false }, Cmd.none

    (**
#### Update function
    *)

    let calculateNeighbourgs (x : int) (y : int) (world : array<array<int>>) =
        [ -1,-1 ; 0,-1 ; 1,-1;
          -1, 0 ;        1, 0;
          -1, 1 ; 0, 1 ; 1, 1; ]
        |> List.map (fun (dx, dy) ->
            let x = x + dx
            let y = y + dy
            if x >= 0
                && x < Const.GRID_SIZE
                && y >= 0
                && y < Const.GRID_SIZE
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
                if timestamp < model.LastFrameTimeMs + (1000. / Const.MAX_FPS) then
                    // printfn "tick part 1"
                    model, Cmd.onAnimationFrame Tick
                else

                    let newWorld =
                        [| for y = 0 to Const.GRID_SIZE - 1 do
                            yield
                                [| for x = 0 to Const.GRID_SIZE - 1 do
                                    let cell = model.World.[y].[x]
                                    if cell = Const.ALIVE then
                                        // A live cell, stay a live cell if it has 2 or 3 neighbougs
                                        match calculateNeighbourgs x y model.World with
                                        | 2
                                        | 3 -> yield Const.ALIVE
                                        | _ -> yield Const.DEAD
                                    else
                                        // A dead cell, becomes a live cell if it has 3 neighbourgs
                                        match calculateNeighbourgs x y model.World with
                                        | 3 -> yield Const.ALIVE
                                        | _ -> yield Const.DEAD |]
                        |]
                    // printfn "tick part 2"
                    { model with World = newWorld
                                 IterationRank = model.IterationRank + 1
                                 LastFrameTimeMs = timestamp } , Cmd.onAnimationFrame Tick
            else
                model, Cmd.none

        | ToggleState ->
            { model with IsRunning = not model.IsRunning }, Cmd.onAnimationFrame Tick

        | MouseDown position ->
            let x = JS.Math.floor (position.X / model.CellWidth) |> int
            let y = JS.Math.floor (position.Y / model.CellWidth) |> int

            let newWorld =
                model.World
                |> Array.mapi (fun index column ->
                    if index = y then
                        column
                        |> Array.mapi (fun index cell ->
                            if index = x then
                                // Inverse cell's state
                                if cell = Const.ALIVE then
                                    Const.DEAD
                                else
                                    Const.ALIVE
                            else
                                cell
                        )
                    else
                        column
                )

            { model with World = newWorld
                         IsDragging = true }, Cmd.none

        | MouseDrag position ->
            let x = JS.Math.floor (position.X / model.CellWidth) |> int
            let y = JS.Math.floor (position.Y / model.CellWidth) |> int

            // PERFORMANCE: First check if the cell isn't already alive
            if model.World.[y].[x] = Const.ALIVE then
                model, Cmd.none
            else
                let newWorld =

                    model.World
                    |> Array.mapi (fun index column ->
                        if index = y then
                            column
                            |> Array.mapi (fun index cell ->
                                if index = x then
                                    Const.ALIVE
                                else
                                    cell
                            )
                        else
                            column
                    )

                { model with World = newWorld }, Cmd.none

        | MouseUp ->
            { model with IsDragging = false }, Cmd.none

        (* hide *)
        | UpdateCanvasSize (width, height) ->
            { model with CanvasWidth = width
                         CanvasHeight = height
                         CellWidth = width / float Const.GRID_SIZE }, Cmd.none
        (* end-hide *)

    (**
#### Views
    *)
    let drawCell (x : int) (y : int) (width : float) (height : float) =
        [ Canvas.FillStyle !^"green"
          Canvas.FillRect (float x * width , float y * height, width, height) ]
        |> Canvas.Batch

    let drawWorld (model : Model) =
        let ySize = model.World.Length
        let xSize = model.World.[0].Length

        let cells =
            [ for y = 0 to ySize - 1 do
                for x = 0 to xSize - 1 do
                    if model.World.[y].[x] = 1 then
                        yield drawCell x y model.CellWidth model.CellWidth
            ]
            |> Canvas.Batch

        let grid =
            let mutable x = model.CellWidth
            let mutable y = model.CellWidth

            [ while x < canvasRef.width do
                yield Canvas.BeginPath
                yield Canvas.MoveTo (x, 0.)
                yield Canvas.LineTo (x, canvasRef.height)
                yield Canvas.Stroke

                x <- x + model.CellWidth

              while y < canvasRef.height do
                yield Canvas.BeginPath
                yield Canvas.MoveTo (0., y)
                yield Canvas.LineTo (canvasRef.width, y)
                yield Canvas.Stroke

                y <- y + model.CellWidth
             ]
            |> Canvas.Batch

        [ cells
          Canvas.StrokeStyle !^"black"
          grid
        ] |> Canvas.Batch

    let paused (model : Model) =
        let text =
            if model.IsRunning then
                "Playing"
            else
                "Paused"
        [ Canvas.FillStyle !^"#222222"
          Canvas.FillRect (model.CanvasWidth - 90., 0., 90., 21.)
          Canvas.FillStyle !^"rgb(170, 245, 163)"
          Canvas.TextBaseline "bottom"
          Canvas.Font "20px Georgia"
          Canvas.FillText
            (Canvas.FillTextBuilder.create text
                |> Canvas.FillTextBuilder.withX (model.CanvasWidth - 75.)
                |> Canvas.FillTextBuilder.withY 21.)
        ] |> Canvas.Batch

    let private renderCanvas (model : Model) width dispatch =
        [ Canvas.ClearRect (0., 0., model.CanvasWidth, model.CanvasHeight)
          Canvas.FillStyle !^"lightgrey"
          Canvas.FillRect (0., 0., model.CanvasWidth, model.CanvasHeight)
          drawWorld model
          paused model]

    open Fable.Helpers.React
    open Fable.Helpers.React.Props

    let private canvasArea model dispatch =
        div [ Style [ Width "500px"
                      Height "500px"
                      Margin "auto" ] ]
            [ ReactResizeDetector.detector [ ReactResizeDetector.HandleHeight
                                             ReactResizeDetector.HandleWidth
                                             ReactResizeDetector.OnResize (fun width height ->
                                                dispatch (UpdateCanvasSize (width, height))
                                            ) ] [ ]
              canvas [ yield HTMLAttr.Width model.CanvasWidth :> IHTMLProp
                       yield HTMLAttr.Height model.CanvasHeight :> IHTMLProp
                       yield Key "game-of-life" :> IHTMLProp
                       if not model.IsRunning
                            && model.IsDragging = false then
                           yield OnMouseDown (fun ev ->
                                let bounds : Browser.ClientRect = !!ev.target?getBoundingClientRect()
                                { X = ev.clientX - bounds.left
                                  Y = ev.clientY - bounds.top }
                                |> MouseDown
                                |> dispatch
                             )  :> IHTMLProp
                       if not model.IsRunning
                            && model.IsDragging = true then
                           yield OnMouseUp (fun _ ->
                                dispatch MouseUp
                             )  :> IHTMLProp
                           yield OnMouseMove (fun ev ->
                                let bounds : Browser.ClientRect = !!ev.target?getBoundingClientRect()
                                { X = ev.clientX - bounds.left
                                  Y = ev.clientY - bounds.top }
                                |> MouseDrag
                                |> dispatch
                             )  :> IHTMLProp
                       yield Ref (fun elt ->
                            if not (isNull elt) && isNull canvasRef then
                                canvasRef <- elt :?> Browser.HTMLCanvasElement

                            if not (isNull elt) && not (isNull canvasRef) then
                                let context = canvasRef.getContext_2d()
                                Canvas.drawOps context (renderCanvas model 0. dispatch)

                         ) :> IHTMLProp ]
                    [ ] ]

    (* hide *)
    let mutable private onKeyPress : Browser.KeyboardEvent -> unit = ignore

    let inline space<'a> = span [ DangerouslySetInnerHTML { __html = "&nbsp;" } ] [ ]
    (* end-hide *)

    let view (model : Model) width dispatch =
        let icon =
            if model.IsRunning then
                Fa.icon Fa.I.Pause
            else
                Fa.icon Fa.I.Play

        div [ ]
            [ GlobalListener.view
                { OnMount = fun () ->
                    onKeyPress <- fun (ev : Browser.KeyboardEvent) ->
                        match ev.key with
                        | " " ->
                            ev.preventDefault()
                            dispatch ToggleState
                        | _ -> ()

                    Browser.window.addEventListener_keypress onKeyPress
                  OnUnmount = fun () ->
                    Browser.window.removeEventListener("keypress", !!onKeyPress)
                    onKeyPress <- ignore }
              Level.level [ ]
                [ Level.item [ ]
                    [ span [ ]
                        [ str "Use your" ]
                      space
                      strong [ ]
                        [ str "space" ]
                      space
                      str " key to Play/Pause the simulation"
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
                [ str "Game of Life" ] ]
          Demo.view model.Demo (float model.Inputs.Size) (DemoMsg >> dispatch)
          div [ Style [ MaxWidth "800px"
                        Margin "auto" ] ]
            [ Elmish.React.Common.lazyView Helpers.View.literateCode (__SOURCE_DIRECTORY__ + "/" + __SOURCE_FILE__) ] ]
