namespace Interlude.Tools

module Utils =
    open System.IO

    let YAVSRG_PATH = 
        Path.Combine(Directory.GetCurrentDirectory(), "..", "..", "..", "..", "..")
        |> Path.GetFullPath

    let ASSETS_PATH = Path.Combine(YAVSRG_PATH, "Interlude", "assets")
    let BUILD_RESOURCES_PATH = Path.Combine(YAVSRG_PATH, "Interlude", "src", "Resources")
    let INTERLUDE = Path.Combine(YAVSRG_PATH, "Interlude", "src")