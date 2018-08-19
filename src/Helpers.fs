module Helpers

module Url =

    #if DEBUG
    let [<Literal>] githubRaw = "/"
    #else
    let [<Literal>] githubRaw = "https://raw.githubusercontent.com/MangelMaxime/Elmish.Canvas/master/src/"
    #endif

module View =

    let inline literateCode filePath =
        LiterateCode.view filePath "/src/" Url.githubRaw

let getFileUrl (filePath : string) =
    let splitAt = filePath.IndexOf("/src/") + 5
    Url.githubRaw + filePath.Substring(splitAt)
