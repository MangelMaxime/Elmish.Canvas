module Demos.SegmentsFollowMouse

[<RequireQualifiedAccess>]
module Particle =

    open System
    open Fable.Core

    (**
We create a record to store all the information about the particle:
    *)
    type Particle =
        { X : float
          Y : float
          Id : int
          Dx : float
          Dy : float
          AngleX : float
          AngleY : float
          SpeedX : float
          SpeedY : float
          Radius : float }

    let private rand = new Random()

    let create index : Particle =
        { X = -50.
          Y = 0.
          Dx = 0.
          Dy = 0.
          Id = index + 1
          AngleX = Math.PI * 2. * rand.NextDouble()
          AngleY = Math.PI * 2. * rand.NextDouble()
          SpeedX = 0.03 * rand.NextDouble() + 0.03
          SpeedY = 0.03 * rand.NextDouble() + 0.03
          Radius = 150. }

    let update (particle : Particle) (width : float) (height : float) (particles : Particle array) (mousePos : Position) =
        if particle.Id > 1 then
            let aim = particles.[particle.Id - 1 - 1]
            let dx = aim.X - particle.X
            let dy = aim.Y - particle.Y

            { particle with X = particle.X + dx * 0.1
                            Y = particle.Y + dy * 0.1
                            Dx = dx
                            Dy = dy }
        else
            if mousePos.X = 0. && mousePos.Y = 0. then
                let dx = width / 2. + Math.Cos(particle.AngleX) * particle.Radius - particle.X
                let dy = height / 2. + Math.Sin(particle.AngleY) * particle.Radius - particle.Y

                { particle with X = width / 2. + Math.Cos(particle.AngleX) * particle.Radius
                                Y = height / 2. + Math.Sin(particle.AngleY) * particle.Radius
                                AngleX = particle.AngleX + particle.SpeedX
                                AngleY = particle.AngleY + particle.SpeedY
                                Dx = dx
                                Dy = dy }

            else
                let dx = mousePos.X - particle.X
                let dy = mousePos.Y - particle.Y

                { particle with X = particle.X + dx * 0.1
                                Y = particle.Y + dy * 0.1
                                Dx = dx
                                Dy = dy }

    let draw (particle : Particle) (total : int) =
        let angle = Math.Atan2(particle.Dy, particle.Dx)
        let scale = Math.Cos(Math.PI / 2. * (float particle.Id / float total))

        [ Canvas.Save
          Canvas.Translate (particle.X, particle.Y)
          Canvas.Rotate angle
          Canvas.Scale (scale, scale)

          Canvas.BeginPath
          Canvas.MoveTo (- 25.0 / 2. * 1.732, - 25. / 2.)
          Canvas.LineTo(0. ,0.)
          Canvas.LineTo(- 25.0 / 2. * 1.732, 25. / 2.)
          Canvas.LineTo(- 25.0 / 2. * 1.2, 0.)
          Canvas.FillStyle (U3.Case1 "white")
          Canvas.Fill

          Canvas.Restore
        ] |> Canvas.Batch

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fable.PowerPack
open Fable.Import
open Fable.Core.JsInterop

type Model =
    { Particles : Particle.Particle array
      Total : int
      FollowSpeed : float
      Size : int
      MousePosition : Position
      Width : float
      Height : float
      ElapsedTime : float }

type Msg =
    | Tick of float
    | MouseMove of Position
    | OnResize of float * float
    | OnError of exn

let init _ =
    { Particles =
        [| for i = 0 to 15 do
            yield Particle.create i
        |]
      Total = 15
      FollowSpeed = 0.1
      Size = 25
      MousePosition = Position.Empty
      Width = 0.
      Height = 0.
      ElapsedTime = 0. }, Cmd.ofMsg (Tick 0.)

let update msg model =
    match msg with
    | Tick elpasedTime ->
        let model =
            { model with ElapsedTime = elpasedTime }

        let particles =
            model.Particles
            |> Array.map (fun particle ->
                Particle.update particle model.Width model.Height model.Particles model.MousePosition
            )

        let delayedCmd elapsedTime =
            promise {
                do! Promise.sleep 16
                return elapsedTime + 16.
            }

        { model with Particles = particles }, Cmd.none

    | MouseMove newPosition ->
        { model with MousePosition = newPosition }, Cmd.none

    | OnResize (width, height) ->
        { model with Width = width
                     Height = height }, Cmd.none

    | OnError error ->
        Browser.console.error error
        model, Cmd.none


let drawParticles (model : Model) =
    [ for particle in model.Particles do
        yield Particle.draw particle model.Total ]
    |> Canvas.Batch

let private renderCanvas (model : Model) dispatch =
    let size : Canvas.Size =
        { Width = model.Width
          Height = model.Height }

    size
    |> Canvas.initialize
    |> Canvas.onTick (fun _ -> dispatch (Tick 0.))
    |> Canvas.onMouseMove (fun ev ->
        let bounds : Browser.ClientRect = !!ev.target?getBoundingClientRect()
        { X = ev.clientX - bounds.left
          Y = ev.clientY - bounds.top }
        |> MouseMove
        |> dispatch
    )
    |> Canvas.draw (Canvas.ClearReact (0., 0., model.Width, model.Height))
    |> Canvas.draw (drawParticles model)
    |> Canvas.render

(* hide *)
let view model dispatch =
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
          div [ Style [ Width "100%"
                        MaxWidth "800px"
                        Height "600px"
                        Margin "auto" ] ]
            [ ReactResizeDetector.detector [ ReactResizeDetector.HandleHeight
                                             ReactResizeDetector.HandleWidth
                                             ReactResizeDetector.OnResize (fun width height ->
                                                dispatch (OnResize (width, height))
                                            ) ] [ ]
              renderCanvas model dispatch ]
          div [ Style [ MaxWidth "800px"
                        Margin "auto" ] ]
            [ Elmish.React.Common.lazyView LiterateCode.view (__SOURCE_DIRECTORY__ + "/" + __SOURCE_FILE__) ] ]
