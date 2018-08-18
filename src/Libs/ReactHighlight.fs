module ReactHighlight

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Import

type Props =
    | ClassName of string
    | InnerHTML of bool

let inline highlight (props: Props list) children : React.ReactElement =
    ofImport "default" "react-highlight" (keyValueList CaseRules.LowerFirst props) children
