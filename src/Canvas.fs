module Canvas

open Fable.Core
open Fable.Import

// The runner class is a port of https://github.com/IceCreamYou/MainLoop.js/
// The following license has been copied from MainLoop.js repo
// Source: https://github.com/IceCreamYou/MainLoop.js/blob/gh-pages/LICENSE.txt

// The MIT License

// Copyright (C) 2016 Isaac Sukin

// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

type Runner() =
    /// The amount of time (in milliseconds) to simulate each time update()
    /// runs. See `MainLoop.setSimulationTimestep()` for details.
    let mutable simulationTimestep = 1000. / 60.

    /// The cumulative amount of in-app time that hasn't been simulated yet.
    /// See the comments inside animate() for details.
    let mutable frameDelta = 0.

    /// The timestamp in milliseconds of the last time the main loop was run.
    /// Used to compute the time elapsed between frames.
    let mutable lastFrameTimeMs = 0.

    /// An exponential moving average of the frames per second.
    let mutable fps = 60.

    /// A factor that affects how heavily to weight more recent seconds'
    /// performance when calculating the average frames per second. Valid values
    /// range from zero to one inclusive. Higher values result in weighting more
    /// recent seconds more heavily.
    let fpsAlpha = 0.9

    /// The minimum duration between updates to the frames-per-second estimate.
    /// Higher values increase accuracy, but result in slower updates.
    let fpsUpdateInterval = 1000.

    /// The timestamp (in milliseconds) of the last time the `fps` moving
    /// average was updated.
    let mutable lastFpsUpdate = 0.

    /// The number of frames delivered since the last time the `fps` moving
    /// average was updated (i.e. since `lastFpsUpdate`).
    let mutable framesSinceLastFpsUpdate = 0

    /// The number of times update() is called in a given frame. This is only
    /// relevant inside of animate(), but a reference is held externally so that
    /// this variable is not marked for garbage collection every time the main
    /// loop runs.
    let mutable numUpdateSteps = 0

    /// Whether we should stop the current update simulation or no. This is only
    /// relevant inside of animate(), but a reference is held externally so that
    /// this variable is not marked for garbage collection every time the main
    /// loop runs.
    let mutable forceBreak = false

    /// The minimum amount of time in milliseconds that must pass since the last
    /// frame was executed before another frame can be executed. The
    /// multiplicative inverse caps the FPS (the default of zero means there is
    /// no cap).
    let mutable minFrameDelay = 0.

    /// Whether the main loop is running.
    let mutable running = false

    /// `true` if `MainLoop.start()` has been called and the most recent time it
    /// was called has not been followed by a call to `MainLoop.stop()`. This is
    /// different than `running` because there is a delay of a few milliseconds
    /// after `MainLoop.start()` is called before the application is considered
    /// "running." This delay is due to waiting for the next frame.
    let mutable started = false

    /// Whether the simulation has fallen too far behind real time.
    /// Specifically, `panic` will be set to `true` if too many updates occur in
    /// one frame. This is only relevant inside of animate(), but a reference is
    /// held externally so that this variable is not marked for garbage
    /// collection every time the main loop runs.
    let mutable panic = false

    /// A function that runs at the beginning of the main loop.
    /// See `MainLoop.setBegin()` for details.
    let mutable ``begin`` = fun _timestamp _frameDelta -> ()

    /// A function that runs updates (i.e. AI and physics).
    /// See `MainLoop.setUpdate()` for details.
    let mutable update = fun _simulationTimestep -> ()

    /// A function that draws things on the screen.
    /// See `MainLoop.setDraw()` for details.
    let mutable draw = fun _simulationTimestep -> ()

    /// A function that runs at the end of the main loop.
    /// See `MainLoop.setEnd()` for details.
    let mutable ``end`` = fun _fps _isPanic -> ()

    /// The ID of the currently executing frame. Used to cancel frames when
    /// stopping the loop.
    let mutable rafHandle = 0.

    member __.SimulationTimestep
        with get() = simulationTimestep
        and set(value) = simulationTimestep <- value


    member __.FPS
        with get() = fps

    member this.MaxAllowedFPS
        with get() = 1000. / minFrameDelay
        and set(value) =
            if value = 0. then
                this.Stop();
            else
                minFrameDelay <- 1000. / value

    member __.ResetFrameDelta () =
        let oldFrameDelta = frameDelta;
        frameDelta <- 0.
        oldFrameDelta

    member __.Begin
        with set(value) = ``begin`` <- value

    member __.Update
        with set(value) = update <- value

    member __.Draw
        with set(value) = draw <- value

    member __.End
        with set(value) = ``end`` <- value

    member this.Start() =
        if not started then
            started <- true

            rafHandle <- Browser.window.requestAnimationFrame(fun timestamp ->
                draw(1.)
                running <- true

                lastFrameTimeMs <- timestamp
                lastFpsUpdate <- timestamp
                framesSinceLastFpsUpdate <- 0

                // Start the main loop.
                rafHandle <- Browser.window.requestAnimationFrame(this.Animate)
            )

    member __.Stop() =
        running <- false
        started <- false
        Browser.window.cancelAnimationFrame(rafHandle)

    member __.IsRunning
        with get() = running

    member this.Animate(timestamp : float) =
        rafHandle <- Browser.window.requestAnimationFrame(this.Animate)

        if (timestamp > lastFrameTimeMs + minFrameDelay) then
            frameDelta <- frameDelta + timestamp - lastFrameTimeMs
            lastFrameTimeMs <- timestamp

            ``begin`` timestamp frameDelta

            if timestamp > lastFpsUpdate + fpsUpdateInterval then
                fps <- fpsAlpha * float framesSinceLastFpsUpdate * 1000. / (timestamp - lastFpsUpdate) + (1. - fpsAlpha) * fps

                lastFpsUpdate <- timestamp
                framesSinceLastFpsUpdate <- 0

            framesSinceLastFpsUpdate <- framesSinceLastFpsUpdate + 1

            numUpdateSteps <- 0
            forceBreak <- false
            while frameDelta >= simulationTimestep && not forceBreak do
                update simulationTimestep
                frameDelta <- frameDelta - simulationTimestep

                numUpdateSteps <- numUpdateSteps + 1
                if numUpdateSteps >= 240 then
                    panic <- true
                    forceBreak <- true

            draw (frameDelta / simulationTimestep)

            ``end`` fps panic

            panic <- false
