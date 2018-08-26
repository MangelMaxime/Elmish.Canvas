module App

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Fable.Import

type Demo =
    | SegmentsFollowMouse of Demos.SegmentsFollowMouse.Model
    | MovingBox of Demos.MovingBox.Model
    | GameOfLife of Demos.GameOfLife.Model

type Page =
    | Home
    | Demo of Demo

type Model =
    { CurrentRoute : Router.Route
      CurrentPage : Page }

type Msg =
    | SegmentsFollowMouseMsg of Demos.SegmentsFollowMouse.Msg
    | MovingBoxMsg of Demos.MovingBox.Msg
    | GameOfLifeMsg of Demos.GameOfLife.Msg

let urlUpdate (result : Option<Router.Route>) model =
    match result with
    | None ->
        Browser.console.error("Error parsing url: " + Browser.window.location.href)
        model, Router.modifyUrl model.CurrentRoute

    | Some page ->
        let model = { model with CurrentRoute = page }
        match page with
        | Router.Home ->
            model, Cmd.none

        | Router.Demo Router.DemoRoute.SegmentsFollowMouse ->
            let (subModel, subCmd) = Demos.SegmentsFollowMouse.init ()
            { model with CurrentPage =
                                subModel
                                |> Demo.SegmentsFollowMouse
                                |> Page.Demo }, Cmd.map SegmentsFollowMouseMsg subCmd

        | Router.Demo Router.DemoRoute.MovingBox ->
            let (subModel, subCmd) = Demos.MovingBox.init ()
            { model with CurrentPage =
                                subModel
                                |> Demo.MovingBox
                                |> Page.Demo }, Cmd.map MovingBoxMsg subCmd

        | Router.Demo Router.DemoRoute.GameOfLife ->
            let (subModel, subCmd) = Demos.GameOfLife.init ()
            { model with CurrentPage =
                                subModel
                                |> Demo.GameOfLife
                                |> Page.Demo }, Cmd.map GameOfLifeMsg subCmd

let init result =
    urlUpdate result { CurrentRoute = Router.Route.Home
                       CurrentPage = Home }

let private update msg model =
    match msg with
    | SegmentsFollowMouseMsg subMsg ->
        match model with
        | { CurrentPage = Page.Demo (Demo.SegmentsFollowMouse subModel) } ->
            let (newModel, newCmd) = Demos.SegmentsFollowMouse.update subMsg subModel
            { model with CurrentPage =
                            newModel
                            |> Demo.SegmentsFollowMouse
                            |> Page.Demo } , Cmd.map SegmentsFollowMouseMsg newCmd
        | _ ->
            model, Cmd.none

    | MovingBoxMsg subMsg ->
        match model with
        | { CurrentPage = Page.Demo (Demo.MovingBox subModel) } ->
            let (newModel, newCmd) = Demos.MovingBox.update subMsg subModel
            { model with CurrentPage =
                            newModel
                            |> Demo.MovingBox
                            |> Page.Demo } , Cmd.map MovingBoxMsg newCmd
        | _ ->
            model, Cmd.none

    | GameOfLifeMsg subMsg ->
        // On page change if the widget isn't the activeone, allow the component to reset it's state ???
        match model with
        | { CurrentPage = Page.Demo (Demo.GameOfLife subModel) } ->
            let (newModel, newCmd) = Demos.GameOfLife.update subMsg subModel
            { model with CurrentPage =
                            newModel
                            |> Demo.GameOfLife
                            |> Page.Demo } , Cmd.map GameOfLifeMsg newCmd
        | _ ->
            model, Cmd.none

let private navbar =
    Navbar.navbar [ Navbar.IsFixedTop
                    Navbar.Color IsLink ]
        [ Navbar.Brand.div [ ]
            [ Navbar.Item.div [ ]
                [ Heading.h4 [ Heading.Props [ Style [ Color "white" ] ] ]
                    [ str "Elmish canvas (demos)" ] ] ] ]

// Helper to generate a menu item
let menuItem label isActive route =
    Menu.Item.li [ Menu.Item.IsActive isActive
                   Menu.Item.Props [ Router.href route ] ]
       [ str label ]

// Helper to generate a sub menu
let subMenu label isActive children =
    li [ ]
       [ Menu.Item.a [ Menu.Item.IsActive isActive ]
            [ str label ]
         ul [ ] children ]

let menu =
    Card.card [ ]
        [ Card.header [ ]
            [ Card.Header.title [ ]
                [ Icon.faIcon [ ]
                    [ Fa.icon Fa.I.Laptop
                      Fa.faLg ]
                  str "Samples" ] ]
          Card.content [ ]
            [ // Menu rendering
              Menu.menu [ ]
                [ Menu.list [ ]
                    [ menuItem "Segments follow mouse" false (Router.Demo Router.DemoRoute.SegmentsFollowMouse)
                      menuItem "Moving box" false (Router.Demo Router.DemoRoute.MovingBox)
                      menuItem "Game of Life" false (Router.Demo Router.DemoRoute.GameOfLife) ] ] ] ]

let about =
    Card.card [ ]
        [ Card.header [ ]
            [ Card.Header.title [ ]
                [ Icon.faIcon [ ]
                    [ Fa.icon Fa.I.Info
                      Fa.faLg ]
                  str "About" ] ]
          Card.content [ ]
            [ a [ ]
                [ Text.span [ Modifiers [ Modifier.TextTransform TextTransform.Italic] ]
                    [ str "Found a bug ?" ] ] ] ]

let private view model dispatch =
    let content =
        match model.CurrentPage with
        | Page.Demo (Demo.SegmentsFollowMouse subModel) ->
            Demos.SegmentsFollowMouse.view subModel (SegmentsFollowMouseMsg >> dispatch)
        | Page.Demo (Demo.MovingBox subModel) ->
            Demos.MovingBox.view subModel (MovingBoxMsg >> dispatch)
        | Page.Demo (Demo.GameOfLife subModel) ->
            Demos.GameOfLife.view subModel (GameOfLifeMsg >> dispatch)
        | Page.Home ->
            str "home"

    div [ ]
        [ navbar
          div [ Class "page-content" ]
            [ div [ Class "sidebar" ]
                [ menu
                  about
                  div [ Style [ Flex "1 1 0"] ]
                    [ ] ]
              div [ Class "main-content" ]
                [ content ] ] ]

open Elmish.React
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

Program.mkProgram init update view
|> Program.toNavigable (parseHash Router.pageParser) urlUpdate
|> Program.withReactUnoptimized "elmish-app"
|> Program.run
