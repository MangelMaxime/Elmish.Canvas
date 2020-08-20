module ReactHighlight

open Fable.Core
open Fable.Core.JsInterop
open Fable.React

type Props =
    | ClassName of string
    | InnerHTML of bool

let inline highlight (props: Props list) children : ReactElement =
    ofImport "default" "react-highlight" (keyValueList CaseRules.LowerFirst props) children
