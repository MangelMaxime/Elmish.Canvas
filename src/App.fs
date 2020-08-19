module App

open Browser.Dom

type Position =
    { X : float
      Y : float }

    static member Empty =
        { X = 0.
          Y = 0. }

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
    let draw (ctx : Browser.Types.CanvasRenderingContext2D) (particle : Particle) (total : int) (width : float) =
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

let mutable particles : Particle.Particle array =
    [|
        for index = 0 to 16 do
            yield Particle.create index
    |]

open Fable.Core
open Fable.Core.JsInterop

let canvas = document.getElementById "app" :?> Browser.Types.HTMLCanvasElement
let context = canvas.getContext_2d()

canvas.width <- window.innerWidth
canvas.height <- window.innerHeight

let runner = Canvas.Runner()

let ``begin`` timestamp frameDelta = ()

let update simulationTimestep =
    particles <-
        particles
        |> Array.map (fun particle ->
            let settings : Particle.Settings =
                { Width = canvas.width
                  Height = canvas.height
                  FollowSpeed = 0.1
                  MousePos =
                    { X = 0.
                      Y = 0. } }
            Particle.update particle particles settings
        )

let draw simulationTimestep =
    context.clearRect(0., 0., canvas.width, canvas.height)
    // Draw the particles
    for particle in particles do
        Particle.draw context particle particles.Length 25.

let ``end`` fps panic =
    context.save()
    context.fillStyle <- !^"black"
    context.font <- "20px Arial, sans-serif"
    context.fillText(string (JS.Math.round(fps)) + " FPS", 0., 20.)
    context.restore()
    // Display FPS
    if panic then
        let discardedTime = runner.ResetFrameDelta()
        console.warn("Runner panicked, probably because the browser tab was put in the background. Discarding " + string discardedTime + "ms");

runner.Begin <- ``begin``
runner.Update <- update
runner.Draw <- draw
runner.End <- ``end``
runner.Start()
