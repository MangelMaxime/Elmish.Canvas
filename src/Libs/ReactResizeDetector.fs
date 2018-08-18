module ReactResizeDetector

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Import

type Props =
    /// Triggered when the value of the Editor changed
    | HandleWidth
    | HandleHeight
    | OnResize of (float -> float -> unit)

let inline detector (props: Props list) children : React.ReactElement =
    ofImport "default" "react-resize-detector" (keyValueList CaseRules.LowerFirst props) children
