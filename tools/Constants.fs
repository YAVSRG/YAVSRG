namespace YAVSRG.CLI

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<AutoOpen>]
module Constants =

    type private PathHelper() =
        static member Path([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
            Path.Combine(path, "..", "..") |> Path.GetFullPath

    let YAVSRG_PATH = PathHelper.Path()
    let TOOLS_PATH = Path.Combine(YAVSRG_PATH, "tools")
    let RELEASES_PATH = Path.Combine(YAVSRG_PATH, "releases")
    let SITE_PATH = Path.Combine(YAVSRG_PATH, "site")
    let WIKI_FILES_PATH = Path.Combine(YAVSRG_PATH, "docs", "wiki")
    let FULL_CHANGELOG_PATH = Path.Combine(YAVSRG_PATH, "docs", "changelog.md")
    let LATEST_CHANGELOG_PATH = Path.Combine(YAVSRG_PATH, "docs", "changelog-latest.md")
    let DOTNET_VERSION = "net9.0"

    let INTERLUDE_ASSETS_PATH = Path.Combine(YAVSRG_PATH, "interlude", "assets")

    let INTERLUDE_BUILD_RESOURCES_PATH =
        Path.Combine(YAVSRG_PATH, "interlude", "src", "Resources")

    let INTERLUDE_SOURCE_PATH = Path.Combine(YAVSRG_PATH, "interlude", "src")
