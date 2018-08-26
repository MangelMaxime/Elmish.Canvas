module Router

open Fable.Import
open Fable.Helpers.React.Props
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

type DemoRoute =
    | SegmentsFollowMouse
    | MovingBox
    | GameOfLife

type Route =
    | Home
    | Demo of DemoRoute

let private toHash page =
    match page with
    | Demo demoPage ->
        match demoPage with
        | SegmentsFollowMouse -> "#segments-follow-mouse"
        | MovingBox -> "#moving-box"
        | GameOfLife -> "#game-of-life"
    | Home -> "#/"

let pageParser: Parser<Route->Route,Route> =
    oneOf [
        map (DemoRoute.SegmentsFollowMouse |> Demo) (s "segments-follow-mouse")
        map (DemoRoute.MovingBox |> Demo) (s "moving-box")
        map (DemoRoute.GameOfLife |> Demo) (s "game-of-life")
        map Home top ]

let href route =
    Href (toHash route)

let modifyUrl route =
    route |> toHash |> Navigation.modifyUrl

let newUrl route =
    route |> toHash |> Navigation.newUrl

let modifyLocation route =
    Browser.window.location.href <- toHash route
