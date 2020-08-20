module LiterateCode

open Fable.Core
open Fable.React
open Fable.React.Props
open Fulma

type Paragraph =
    | Code of string
    | Content of string

type Tag =
    | Code
    | Content
    | Hide

type ParserState =
    { Paragraphs : Paragraph list
      CurrentBlock : Tag
      CapturedLines : string list }

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None


let parseText (text : string) =
    let lines = text.Split('\n') |> Array.toList

    let rec parse (lines : string list) (state : ParserState) =
        match lines with
        | line::rest ->
            match state.CurrentBlock with
            | Tag.Hide ->
                let newState =
                    if line.Trim() = "(* end-hide *)" then
                        { state with CapturedLines = [ ]
                                     CurrentBlock = Tag.Code }
                    else
                        state

                parse rest newState
            | Tag.Content ->
                let newState =
                    if line.Trim() = "*)" then
                        { state with CapturedLines = []
                                     Paragraphs =
                                        state.CapturedLines
                                        |> String.concat "\n"
                                        |> Paragraph.Content
                                        |> List.singleton
                                        |> List.append state.Paragraphs
                                     CurrentBlock = Tag.Code }
                    else
                        { state with CapturedLines =
                                        line
                                        |> List.singleton
                                        |> List.append state.CapturedLines }

                parse rest newState
            | Tag.Code ->
                let newState =
                    if line.Trim() = "(**" then
                        { state with CapturedLines = []
                                     Paragraphs =
                                        state.CapturedLines
                                        |> String.concat "\n"
                                        |> Paragraph.Code
                                        |> List.singleton
                                        |> List.append state.Paragraphs
                                     CurrentBlock = Tag.Content }
                    else if line.Trim() = "(* hide *)" then
                        { state with CapturedLines = []
                                     Paragraphs =
                                        state.CapturedLines
                                        |> String.concat "\n"
                                        |> Paragraph.Code
                                        |> List.singleton
                                        |> List.append state.Paragraphs
                                     CurrentBlock = Tag.Hide }
                    else
                        { state with CapturedLines =
                                        line
                                        |> List.singleton
                                        |> List.append state.CapturedLines }
                parse rest newState

        | [] ->
            match state.CurrentBlock with
            | Tag.Code ->
                state.CapturedLines
                |> String.concat "\n"
                |> Paragraph.Code
                |> List.singleton
                |> List.append state.Paragraphs
            | Tag.Content ->
                state.CapturedLines
                |> String.concat "\n"
                |> Paragraph.Content
                |> List.singleton
                |> List.append state.Paragraphs
            | Hide ->
                state.Paragraphs

    parse lines { Paragraphs = []
                  CurrentBlock = Tag.Code
                  CapturedLines = [] }
    |> List.filter (function
        | Paragraph.Content "" -> false
        | Paragraph.Code "" -> false
        | _ -> true
    )

type Props =
    { FilePath : string
      Separator : string
      Url : string }

type State =
    { Content : Paragraph list }

type LiterateCode (props) =
    inherit Component<Props, State>(props)
    do base.setInitState({ Content = [] })

    member this.fileUrl
        with get () =
            let splitAt = this.props.FilePath.IndexOf(this.props.Separator) + this.props.Separator.Length

            this.props.Url + this.props.FilePath.Substring(splitAt)

    override this.componentDidMount() =

        promise {
            let! res = Fetch.fetch this.fileUrl []
            let! text = res.text()
            let parsed = parseText text

            this.setContent(parsed)
        }
        |> Promise.start

    member this.setContent(text) =
        this.setState(fun state props -> { state with Content = text })

    override this.render() =
        let text =
            this.state.Content
            |> List.map (function
                | Paragraph.Content text ->
                    Render.contentFromMarkdown [ ]
                        text
                | Paragraph.Code text ->
                    // ReactHighlight.highlight [ ReactHighlight.ClassName "fsharp" ]
                    //     [ str text ]
                    pre [ ]
                        [ code [ ]
                            [ str text ] ]
            )

        let sourceCodeUrl =
            Text.div [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ a [ Href this.fileUrl ]
                    [ str "Source code" ] ]

        Content.content [ ]
            (sourceCodeUrl::text)

let view filePath separator url =
    ofType<LiterateCode,_,_>
        { FilePath = filePath
          Separator = separator
          Url = url }
        [ ]
