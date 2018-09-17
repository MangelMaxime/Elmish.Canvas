(* hide *)
module Demos.SegmentsFollowMouse
(* end-hide *)

module Canvas =

    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Fable.Helpers.React
    open Fable.Helpers.React.Props

    [<Pojo>]
    type Props<'Model, 'Msg> =
        { OnMount : unit -> unit
          OnUnmount: unit -> unit
          Height : float
          Width : float
          Props : IHTMLProp list
          Renderer : Browser.CanvasRenderingContext2D -> unit }


    let initialProps : Props<'Model, 'Msg> =
        { OnMount = ignore
          OnUnmount = ignore
          Height = 300.
          Width = 150.
          Props = [ ]
          Renderer = fun _ -> () }

    [<Pojo>]
    type State = obj

    type CanvasComponents<'Model, 'Msg>(props) =
        inherit React.Component<Props<'Model, 'Msg>, State>(props)
        do base.setInitState(null)

        member this.canvasRef with get () = unbox<Browser.HTMLCanvasElement> this?refs?canvas

        override __.componentWillUnmount () =
            props.OnUnmount ()

        override __.componentDidMount () =
            props.OnMount ()

        override this.componentDidUpdate (_prevProps, _prevState) =
            this.props.Renderer(this.canvasRef.getContext_2d())

        override this.render () =
            let props =
                [ HTMLAttr.Width this.props.Width :> IHTMLProp
                  HTMLAttr.Height this.props.Height :> IHTMLProp
                  HTMLAttr.Custom("ref", "canvas") :> IHTMLProp ]
                @ this.props.Props

            canvas props
                [ ]

    let view props =
        ofType<CanvasComponents<'Model, 'Msg>,_,_> props [ ]

(**
### Particle module

We first create a `Particle` module, which will contains all the logic about particles.
*)
[<RequireQualifiedAccess>]
module Particle =

(* hide *)
    open System
    open Fable.Core
    open Fable.Import

(* end-hide *)

    (**
We create a record to store all the information about the particle:
    *)
    type Particle =
        { /// X coordinate
          X : float
          /// Y coordinate
          Y : float
          /// Unique Id, it's index based so we can find the previous particle
          Id : int
          /// Force applied, to the X coordinate
          Dx : float
          /// Force applied, to the Y coordiate
          Dy : float
          AngleX : float
          AngleY : float
          SpeedX : float
          SpeedY : float
          Radius : float }

    (**
When creating a particle, we use random values. This allow us to have a nice animation for waiting mouse events.
    *)

    let private rand = new Random()

    let create index : Particle =
        { X = -50.
          Y = -50.
          Dx = 0.
          Dy = 0.
          Id = index + 1
          AngleX = Math.PI * 2. * rand.NextDouble()
          AngleY = Math.PI * 2. * rand.NextDouble()
          SpeedX = 0.03 * rand.NextDouble() + 0.03
          SpeedY = 0.03 * rand.NextDouble() + 0.03
          Radius = 150. }

    (**
We use a record for the settings, this simply the `update` signature. And allow us to easily add new settings to the demo.
    *)
    type Settings =
        { Width : float
          Height : float
          FollowSpeed : float
          MousePos : Position }

    (**
In the update function, we handle the "pysics" of our particles
    *)
    let update (particle : Particle) (particles : Particle array) (settings : Settings) =
        // If this is not the first particle, then it follows the previous particle
        if particle.Id > 1 then
            // Target we are aiming at
            let aim = particles.[particle.Id - 1 - 1]
            let dx = aim.X - particle.X
            let dy = aim.Y - particle.Y

            { particle with X = particle.X + dx * settings.FollowSpeed
                            Y = particle.Y + dy * settings.FollowSpeed
                            Dx = dx
                            Dy = dy }
        else
            // If the mouse never moved, then create random mouvements
            if settings.MousePos.X = 0. && settings.MousePos.Y = 0. then
                let dx = settings.Width / 2. + Math.Cos(particle.AngleX) * particle.Radius - particle.X
                let dy = settings.Height / 2. + Math.Sin(particle.AngleY) * particle.Radius - particle.Y

                { particle with X = settings.Width / 2. + Math.Cos(particle.AngleX) * particle.Radius
                                Y = settings.Height / 2. + Math.Sin(particle.AngleY) * particle.Radius
                                AngleX = particle.AngleX + particle.SpeedX
                                AngleY = particle.AngleY + particle.SpeedY
                                Dx = dx
                                Dy = dy }

            else
                // Follow the mouse
                let dx = settings.MousePos.X - particle.X
                let dy = settings.MousePos.Y - particle.Y

                { particle with X = particle.X + dx * settings.FollowSpeed
                                Y = particle.Y + dy * settings.FollowSpeed
                                Dx = dx
                                Dy = dy }

    (**
We describe the actions needed for drawing a particle on the canvas
    *)
    let draw (ctx : Browser.CanvasRenderingContext2D) (particle : Particle) (total : int) (width : float) =
        let angle = Math.Atan2(particle.Dy, particle.Dx)
        let scale = Math.Cos(Math.PI / 2. * (float particle.Id / float total))

        ctx.save()
        ctx.translate(particle.X, particle.Y)
        ctx.rotate(angle)
        ctx.scale(scale, scale)

        ctx.beginPath()
        ctx.moveTo(-width / 2. * 1.732, -width / 2.)
        ctx.lineTo(0. ,0.)
        ctx.lineTo(-width / 2. * 1.732, width / 2.)
        ctx.lineTo(-width / 2. * 1.2, 0.)
        ctx.fillStyle <- U3.Case1 "white"
        ctx.fill()

        ctx.restore()

(**
### Elmish component

From here this is a standard Elmish component
*)
module Demo =

    open Elmish
    open Fable.PowerPack
    open Fable.Import
    open Fable.Core.JsInterop
    open Fable.Helpers.React.Props

    (**
#### Model
    *)

    type Settings =
        { /// Size of a particle
          Size : int
          /// Speed apply to the first particle (the one who follow the mouse)
          FollowSpeed : float
          /// Number of particles to draw
          NumOfSegments : int
          CanvasWidth : float
          CanvasHeight : float }

    type Model =
        { /// Particles datas
          /// We use array because they are smaller in memory
          Particles : Particle.Particle array
          Settings : Settings
          MousePosition : Position
          LastFrameTimeMs : float }

    (**
#### Msg
    *)
    type Msg =
        /// The tick message is trigger at 60fps, and is responsible for the animation trigger
        | Tick of float
        /// Event triggered when Mouse move over the canvas
        | MouseMove of Position
        (* hide *)
        | UpdateCanvasSize of float * float
        | UpdateNumOfSegments of int
        | UpdateFollowSpeed of float
        | UpdateSize of int
        (* end-hide *)

    (**
#### Init function
    *)
    let init (settings : Settings) =
        { Particles =
            [|
                for index = 0 to settings.NumOfSegments do
                    yield Particle.create index
            |]
          Settings = settings
          MousePosition =
            { X = 0.
              Y = 0. }
          LastFrameTimeMs = 0. }, Cmd.ofMsg (Tick 0.)

    module Cmd =

        let onAnimationFrame (messageCtor : float -> 'Msg) =
            let handler dispatch =
                Browser.window.requestAnimationFrame(fun timestamp ->
                    messageCtor timestamp |> dispatch
                ) |> ignore
            [ handler ]

    (**
#### Update function
    *)
    let update msg model =
        match msg with
        // Update the animation
        | Tick timestamp ->
            if timestamp < model.LastFrameTimeMs + (1000. / 30.) then
                model, Cmd.onAnimationFrame Tick
            else
                // Update all particles positions
                let particles =
                    model.Particles
                    |> Array.map (fun particle ->
                        let settings : Particle.Settings =
                            { Width = model.Settings.CanvasWidth
                              Height = model.Settings.CanvasHeight
                              FollowSpeed = model.Settings.FollowSpeed
                              MousePos = model.MousePosition }
                        Particle.update particle model.Particles settings
                    )

                { model with Particles = particles
                             LastFrameTimeMs = timestamp }, Cmd.onAnimationFrame Tick

        // Update the mouse position
        | MouseMove newPosition ->
            { model with MousePosition = newPosition }, Cmd.none

        (* hide *)
        | UpdateCanvasSize (width, height) ->
            { model with Settings =
                            { model.Settings with CanvasWidth = width
                                                  CanvasHeight = height } }, Cmd.none

        | UpdateNumOfSegments newNum ->
            { model with Particles =
                            [|
                                for index = 0 to newNum do
                                    yield Particle.create index
                            |]
                         Settings =
                            { model.Settings with NumOfSegments = newNum }
                         MousePosition =
                            { X = 0.
                              Y = 0. } }, Cmd.none

        | UpdateFollowSpeed newSpeed ->
            { model with Settings =
                            { model.Settings with FollowSpeed = newSpeed } }, Cmd.none

        | UpdateSize newSize ->
            { model with Settings =
                            { model.Settings with Size = newSize } }, Cmd.none
        (* end-hide *)

    (**
#### Views
    *)
    let renderer (model : Model) (ctx : Browser.CanvasRenderingContext2D) =
        // Clear previous frame
        ctx.clearRect(0., 0., model.Settings.CanvasWidth, model.Settings.CanvasHeight)
        // Draw the particles
        for particle in model.Particles do
            Particle.draw ctx particle model.Particles.Length (float model.Settings.Size)

    let view (model : Model) dispatch =
        Canvas.view
            { Canvas.initialProps
                with Width = model.Settings.CanvasWidth
                     Height = model.Settings.CanvasHeight
                     Props =
                        [ OnMouseMove (fun ev ->
                            let bounds : Browser.ClientRect = !!ev.target?getBoundingClientRect()
                            { X = ev.clientX - bounds.left
                              Y = ev.clientY - bounds.top }
                            |> MouseMove
                            |> dispatch
                        ) ]
                     Renderer = renderer model }

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
    | ChangeNum of string
    | ChangeFollowSpeed of string
    | ChangeSize of string
    | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
    | FinichChangeNum
    | FinichChangeFollowSpeed
    | FinichChangeSize
    | DemoMsg of Demo.Msg

let init _ =
    let demoSettings : Demo.Settings =
        { Size = 25
          FollowSpeed = 0.1
          NumOfSegments = 16
          CanvasWidth = 0.
          CanvasHeight = 0. }
    let (demoModel, demoCmd) = Demo.init demoSettings

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

    | ChangeNum newNum ->
        let (debouncerModel, debouncerCmd) =
            model.Debouncer
            |> Debouncer.bounce (TimeSpan.FromSeconds 0.5) "change_num" FinichChangeNum

        { model with Inputs =
                        { model.Inputs with NumOfSegments = newNum }
                     Debouncer = debouncerModel }, Cmd.map DebouncerSelfMsg debouncerCmd

    | ChangeFollowSpeed newSpeed ->
        let (debouncerModel, debouncerCmd) =
            model.Debouncer
            |> Debouncer.bounce (TimeSpan.FromSeconds 0.5) "change_follow_speed" FinichChangeFollowSpeed

        { model with Inputs =
                        { model.Inputs with FollowSpeed = newSpeed }
                     Debouncer = debouncerModel }, Cmd.map DebouncerSelfMsg debouncerCmd

    | ChangeSize newSize ->
        let (debouncerModel, debouncerCmd) =
            model.Debouncer
            |> Debouncer.bounce (TimeSpan.FromSeconds 0.5) "change_size" FinichChangeFollowSpeed

        { model with Inputs =
                        { model.Inputs with Size = newSize }
                     Debouncer = debouncerModel }, Cmd.map DebouncerSelfMsg debouncerCmd

    | FinichChangeNum ->
        model, int model.Inputs.NumOfSegments
                |> Demo.Msg.UpdateNumOfSegments
                |> DemoMsg
                |> Cmd.ofMsg

    | FinichChangeFollowSpeed ->
        model, float model.Inputs.FollowSpeed
                |> Demo.Msg.UpdateFollowSpeed
                |> DemoMsg
                |> Cmd.ofMsg

    | FinichChangeSize ->
        model, int model.Inputs.Size
                |> Demo.Msg.UpdateSize
                |> DemoMsg
                |> Cmd.ofMsg

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
                    [ Input.input [ Input.OnChange (fun ev ->
                                                    ev.Value
                                                    |> ChangeNum
                                                    |> dispatch)
                                    Input.Value (string model.NumOfSegments)

                                    Input.Props [ Min "5"
                                                  Max "30"
                                                  Step "1"
                                                  HTMLAttr.Type "range" ] ] ] ] ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Follow speed"
                  Control.div [ ]
                    [ Input.number [ Input.OnChange (fun ev ->
                                                    ev.Value
                                                    |> ChangeFollowSpeed
                                                    |> dispatch)
                                     Input.Value (string model.FollowSpeed)
                                     Input.Props [ Min "0.05"
                                                   Max "0.25"
                                                   Step "0.01"
                                                   HTMLAttr.Type "range" ] ] ] ] ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Size"
                  Control.div [ ]
                    [ Input.number [ Input.OnChange (fun ev ->
                                                    ev.Value
                                                    |> ChangeSize
                                                    |> dispatch)
                                     Input.Value (string model.Size)
                                     Input.Props [ Min "5"
                                                   Max "40"
                                                   Step "2"
                                                   HTMLAttr.Type "range" ] ] ] ] ] ]

let view (model : Model) dispatch =
    div [ Style [ Margin "1em" ] ]
        [ Content.content [ ]
            [ Heading.h4 [ ]
                [ str "Segments mouse following" ]
              Heading.p [ Heading.Is6
                          Heading.IsSubtitle ]
                [ str "This project has been ported from "
                  a [ Href "https://codepen.io/anon/pen/PBrVdO"
                      Class "is-italic" ]
                    [ str "this codepen" ] ] ]
          div [ Class "demo" ]
            [ settings model.Inputs dispatch
              div [ ]
                [ ReactResizeDetector.detector [ ReactResizeDetector.HandleHeight
                                                 ReactResizeDetector.HandleWidth
                                                 ReactResizeDetector.OnResize (fun width height ->
                                                    dispatch (OnResize (width, height))
                                                ) ] [ ]
                  Demo.view model.Demo (DemoMsg >> dispatch) ] ]
          div [ Style [ MaxWidth "800px"
                        Margin "auto" ] ]
            [ Elmish.React.Common.lazyView Helpers.View.literateCode (__SOURCE_DIRECTORY__ + "/" + __SOURCE_FILE__) ] ]
