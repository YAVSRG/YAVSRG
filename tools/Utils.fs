namespace YAVSRG.CLI

open System.IO
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

module Utils =

    type PathHelper() =
        static member Path([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
            Path.Combine(path, "..", "..") |> Path.GetFullPath

    let YAVSRG_PATH = PathHelper.Path()
    let TOOLS_PATH = Path.Combine(YAVSRG_PATH, "tools")
    let ASSETS_PATH = Path.Combine(YAVSRG_PATH, "interlude", "assets")
    let SITE_PATH = Path.Combine(YAVSRG_PATH, "site")

    let BUILD_RESOURCES_PATH =
        Path.Combine(YAVSRG_PATH, "interlude", "src", "Resources")

    let INTERLUDE_SOURCE_PATH = Path.Combine(YAVSRG_PATH, "interlude", "src")

    let exec (cmd: string) (args: string) =
        Process
            .Start(ProcessStartInfo(cmd, args, WorkingDirectory = YAVSRG_PATH))
            .WaitForExit()

    let eval (cmd: string) (args: string) : string =
        let p =
            Process.Start(ProcessStartInfo(cmd, args, WorkingDirectory = YAVSRG_PATH, RedirectStandardOutput = true))

        let output = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        output.Trim()

    let exec_at (path: string) (cmd: string) (args: string) =
        Process
            .Start(ProcessStartInfo(cmd, args, WorkingDirectory = path))
            .WaitForExit()

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