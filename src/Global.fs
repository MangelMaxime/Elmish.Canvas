[<AutoOpen>]
module Global


type Position =
    { X : float
      Y : float }

    static member Empty =
        { X = 0.
          Y = 0. }

module Render =

    open Fable.Import
    open Fulma
    open Fable.Helpers.React.Props

    let converter = Showdown.Globals.Converter.Create()

    type DangerousInnerHtml =
        { __html : string }

    let contentFromMarkdown options str =
        Content.content
            [ yield! options
              yield Content.Props [ DangerouslySetInnerHTML { __html =  converter.makeHtml str } ] ]
            [ ]
