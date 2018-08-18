module App

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Fable.Import

type Demo =
    | SegmentsFollowMouse of Demos.SegmentsFollowMouse.Model

type Page =
    | Home
    | Demo of Demo

type Model =
    { CurrentRoute : Router.Route
      CurrentPage : Page }

type Msg =
    | SegmentsFollowMouseMsg of Demos.SegmentsFollowMouse.Msg

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

let private navbar =
    Navbar.navbar [ Navbar.IsFixedTop
                    Navbar.Color IsPrimary ]
        [ Navbar.Brand.div [ ]
            [ Navbar.Item.div [ ]
                [ Heading.h4 [ ]
                    [ str "Elmish canvas (demos)" ] ] ] ]

// Helper to generate a menu item
let menuItem label isActive =
    Menu.Item.li [ Menu.Item.IsActive isActive
                   Menu.Item.Props [ (Router.href (Router.Demo Router.DemoRoute.SegmentsFollowMouse)) ] ]
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
                [ Menu.label [ ] [ str "General" ]
                  Menu.list [ ]
                    [ menuItem "Dashboard" false
                      menuItem "Customers" false ]
                  Menu.label [ ] [ str "Administration" ]
                  Menu.list [ ]
                    [ menuItem "Team Settings" false
                      subMenu "Manage your Team" true
                            [ menuItem "Members" false
                              menuItem "Plugins" false
                              menuItem "Add a member" false ] ]
                  Menu.label [ ] [ str "Transactions" ]
                  Menu.list [ ]
                    [ menuItem "Payments" false
                      menuItem "Transfers" false
                      menuItem "Balance" false ] ] ] ]

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
|> Program.withReact "elmish-app"
|> Program.run
