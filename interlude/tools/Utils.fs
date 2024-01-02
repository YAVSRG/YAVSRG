namespace Interlude.Tools

module Utils =
    open System.IO
    open System.Diagnostics
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    type PathHelper() =
        static member Path([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
            Path.Combine(path, "..", "..", "..") |> Path.GetFullPath

    let YAVSRG_PATH = PathHelper.Path()
    let TOOLS_PATH = Path.Combine(YAVSRG_PATH, "interlude", "tools")
    let ASSETS_PATH = Path.Combine(YAVSRG_PATH, "interlude", "assets")

    let BUILD_RESOURCES_PATH =
        Path.Combine(YAVSRG_PATH, "interlude", "src", "Resources")

    let INTERLUDE_SOURCE_PATH = Path.Combine(YAVSRG_PATH, "interlude", "src")

    let exec (cmd: string) (args: string) =
        Process.Start(ProcessStartInfo(cmd, args)).WaitForExit()
