module Canvas

open Fable.Core
open Fable.Import
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish

module JS =

    [<Emit("undefined")>]
    let inline undefined<'a> : 'a = jsNative

module FillTextBuilder =

    type FillTextBuilder =
        { Text : string
          X : float
          Y : float
          MaxWidth: float option }

    let create text =
        { Text = text
          X = 0.
          Y = 0.
          MaxWidth = None }

    let withX x (builder : FillTextBuilder) =
        { builder with X = x }

    let withY y (builder : FillTextBuilder) =
        { builder with Y = y }

    let withMaxWidth width (builder : FillTextBuilder) =
        { builder with MaxWidth = Some width }

type DrawOp =
    | FillStyle of U3<string, Browser.CanvasGradient, Browser.CanvasPattern>
    | Font of string
    | GlobalAlpha of float
    | GlobalCompositeOperation of string
    | LineCap of string
    | LineDashOffset of float
    | LineJoin of string
    | LineWidth of float
    | MiterLimit of float
    | MsFillRule of string
    | MsImageSmoothingEnabled of bool
    | ShadowBlur of float
    | ShadowColor of string
    | ShadowOffsetX of float
    | ShadowOffsetY of float
    | StrokeStyle of U3<string, Browser.CanvasGradient, Browser.CanvasPattern>
    | TextAlign of string
    | TextBaseline of string
    | Arc of (float * float * float * float * float * bool)
    | ArcTo of (float * float * float * float * float)
    | BeginPath
    | BezierCurveTo of (float * float * float * float * float * float)
    | ClearRect of (float * float * float * float)
    | Clip of string
    | ClosePath
    | CreateImageData of U2<float, Browser.ImageData> * float
    // | CreateLinearGradient of float * float * float * float -> CanvasGradient
    // | CreatePattern of U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement> * string -> CanvasPattern
    // | CreateRadialGradient of float * float * float * float * float * float -> CanvasGradient
    | DrawImage of (U3<Browser.HTMLImageElement, Browser.HTMLCanvasElement, Browser.HTMLVideoElement> * float * float * float * float * float * float * float * float)
    | Fill
    | FillRect of (float * float * float * float)
    | FillText of FillTextBuilder.FillTextBuilder
    // | GetImageData of float * float * float * float -> ImageData
    // | GetLineDash
    // | IsPointInPath of float * float * string -> bool
    | LineTo of (float * float)
    // | MeasureText of string -> Browser.TextMetrics
    | MoveTo of (float * float)
    | PutImageData of (Browser.ImageData * float * float * float * float * float * float)
    | QuadraticCurveTo of (float * float * float * float)
    | Rect of (float * float * float * float)
    | Restore
    | Rotate of float
    | Save
    | Scale of (float * float)
    | SetLineDash of ResizeArray<float>
    | SetTransform of (float * float * float * float * float * float)
    | Stroke
    | StrokeRect of (float * float * float * float)
    | StrokeText of (string * float * float * float)
    | Transform of (float * float * float * float * float * float)
    | Translate of (float * float)
    | Batch of DrawOp list

let rec drawOps (ctx : Browser.CanvasRenderingContext2D) (ops : DrawOp list) =
    for op in ops do
        match op with
        | FillStyle opts -> ctx.fillStyle <- opts
        | Font opts -> ctx.font <- opts
        | GlobalAlpha opts -> ctx.globalAlpha <- opts
        | GlobalCompositeOperation opts -> ctx.globalCompositeOperation <- opts
        | LineCap opts -> ctx.lineCap <- opts
        | LineDashOffset opts -> ctx.lineDashOffset <- opts
        | LineJoin opts -> ctx.lineJoin <- opts
        | LineWidth opts -> ctx.lineWidth <- opts
        | MiterLimit opts -> ctx.miterLimit <- opts
        | MsFillRule opts -> ctx.msFillRule <- opts
        | MsImageSmoothingEnabled opts -> ctx.msImageSmoothingEnabled <- opts
        | ShadowBlur opts -> ctx.shadowBlur <- opts
        | ShadowColor opts -> ctx.shadowColor <- opts
        | ShadowOffsetX opts -> ctx.shadowOffsetX <- opts
        | ShadowOffsetY opts -> ctx.shadowOffsetY <- opts
        | StrokeStyle opts -> ctx.strokeStyle <- opts
        | TextAlign opts -> ctx.textAlign <- opts
        | TextBaseline opts -> ctx.textBaseline <- opts
        | Arc (x, y, radius, startAngle, endAngle, anticlockwise) -> ctx.arc(x, y, radius, startAngle, endAngle, anticlockwise)
        | ArcTo opts -> ctx.arcTo opts
        | BeginPath -> ctx.beginPath()
        | BezierCurveTo opts -> ctx.bezierCurveTo opts
        | ClearRect opts -> ctx.clearRect opts
        | Clip opts -> ctx.clip opts
        | ClosePath -> ctx.closePath()
        // | CreateImageData opts -> ctx.createImageData opts
        // | CreateLinearGradient opts -> ctx.createLinearGradient opts
        // | CreatePattern opts -> ctx.createPattern opts
        // | CreateRadialGradient opts -> ctx.createRadialGradient opts
        // | DrawImage opts -> ctx.drawImage opts
        | Fill -> ctx.fill()
        | FillRect opts -> ctx.fillRect opts
        | FillText builder ->
            ctx.fillText(
                builder.Text,
                builder.X,
                builder.Y,
                maxWidth= defaultArg builder.MaxWidth JS.undefined
            )
        // | GetImageData opts -> ctx.getImageData opts
        // | GetLineDas opts -> ctx.getLineDas opts
        // | IsPointInPath opts -> ctx.isPointInPath opts
        | LineTo opts -> ctx.lineTo opts
        // | MeasureText opts -> ctx.measureText opts
        | MoveTo opts -> ctx.moveTo opts
        // | PutImageData opts -> ctx.putImageData opts
        | QuadraticCurveTo opts -> ctx.quadraticCurveTo opts
        | Rect opts -> ctx.rect opts
        | Restore -> ctx.restore()
        | Rotate opts -> ctx.rotate opts
        | Save -> ctx.save()
        | Scale opts -> ctx.scale opts
        | SetLineDash opts -> ctx.setLineDash opts
        | SetTransform opts -> ctx.setTransform opts
        | Stroke -> ctx.stroke()
        | StrokeRect opts -> ctx.strokeRect opts
        // | StrokeText opts -> ctx.strokeText opts
        | Transform opts -> ctx.transform opts
        | Translate opts -> ctx.translate opts
        | Batch ops -> drawOps ctx ops
        | x -> Browser.console.warn(sprintf "Operation %A isn't supported yet" x)

type private Props =
    | Height of float
    | Width of float
    | DrawOps of DrawOp array
    | OnTick of ((float * float) -> unit)
    | IsPlaying of bool
    | OnMouseMove of (React.MouseEvent -> unit)
    | Style of HTMLAttr
    | MaxFPS of int

open Fable.Core.JsInterop

type Size =
    { Width : float
      Height : float }

type CanvasBuilder =
    { Size : Size
      DrawOps : DrawOp list
      IsPlaying : bool
      OnTick : (float * float) -> unit
      OnMouseMove : React.MouseEvent -> unit
      Style : CSSProp list
      MaxFPS : int }

let inline private canvas (props: Props list) : React.ReactElement =
    ofImport "default" "./js/react_canvas.js" (keyValueList CaseRules.LowerFirst props) [ ]

let initialize (size : Size) : CanvasBuilder =
    { Size = size
      DrawOps = []
      OnTick = ignore
      IsPlaying = true
      OnMouseMove = ignore
      Style = []
      MaxFPS = 30 }

let draw (drawOp : DrawOp) (builder : CanvasBuilder) : CanvasBuilder =
    { builder with DrawOps = builder.DrawOps @ [drawOp] }

let playing value (builder : CanvasBuilder) : CanvasBuilder =
    { builder with IsPlaying = value }

let onTick callback (builder : CanvasBuilder) : CanvasBuilder =
    { builder with OnTick = callback }

let onMouseMove callback (builder : CanvasBuilder) : CanvasBuilder =
    { builder with OnMouseMove = callback}

let withStyle style (builder : CanvasBuilder) : CanvasBuilder =
    { builder with Style = style }

let withMaxFPS fps (builder : CanvasBuilder) : CanvasBuilder =
    { builder with MaxFPS = fps }

let render (builder : CanvasBuilder) =
    canvas [ Width builder.Size.Width
             Height builder.Size.Height
             DrawOps (List.toArray builder.DrawOps)
             OnTick builder.OnTick
             IsPlaying builder.IsPlaying
             OnMouseMove builder.OnMouseMove
             Style !!(keyValueList CaseRules.LowerFirst builder.Style)
             MaxFPS builder.MaxFPS ]
