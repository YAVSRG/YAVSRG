namespace Interlude.Tools

module Utils =
    open System.IO
    open System.Diagnostics
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    type PathHelper() =
        static member Path([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
            Path.Combine(path, "..", "..") |> Path.GetFullPath

    let YAVSRG_PATH = PathHelper.Path()
    let TOOLS_PATH = Path.Combine(YAVSRG_PATH, "tools")
    let ASSETS_PATH = Path.Combine(YAVSRG_PATH, "interlude", "assets")

    let BUILD_RESOURCES_PATH =
        Path.Combine(YAVSRG_PATH, "interlude", "src", "Resources")

    let INTERLUDE_SOURCE_PATH = Path.Combine(YAVSRG_PATH, "interlude", "src")

    let exec (cmd: string) (args: string) =
        Process.Start(ProcessStartInfo(cmd, args)).WaitForExit()

    let rec walk_fs_files (dir: string) : (string * string) seq =
        seq {
            for file in Directory.GetFiles(dir) do
                if Path.GetExtension(file).ToLower() = ".fs" then
                    yield file, File.ReadAllText file

            for dir in Directory.GetDirectories(dir) do
                let name = Path.GetFileName dir

                if name <> "bin" && name <> "obj" then
                    yield! walk_fs_files dir
        }