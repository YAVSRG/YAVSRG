namespace YAVSRG.CLI

open System.IO
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

module [<AutoOpen>] Constants =

    type private PathHelper() =
        static member Path([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string) : string =
            Path.Combine(path, "..", "..") |> Path.GetFullPath

    let YAVSRG_PATH = PathHelper.Path()
    let TOOLS_PATH = Path.Combine(YAVSRG_PATH, "tools")
    let ASSETS_PATH = Path.Combine(YAVSRG_PATH, "interlude", "assets")
    let SITE_PATH = Path.Combine(YAVSRG_PATH, "site")
    let DOTNET_VERSION = "net9.0"

    let BUILD_RESOURCES_PATH =
        Path.Combine(YAVSRG_PATH, "interlude", "src", "Resources")

    let INTERLUDE_SOURCE_PATH = Path.Combine(YAVSRG_PATH, "interlude", "src")

module SourceFiles =

    let rec walk_fs_files (directory: string) : string seq =
        seq {
            for file in Directory.GetFiles(directory) do
                if Path.GetExtension(file).ToLower() = ".fs" then
                    yield file

            for subdirectory in Directory.GetDirectories(directory) do
                let subdirectory_name = Path.GetFileName subdirectory

                if subdirectory_name <> "bin" && subdirectory_name <> "obj" then
                    yield! walk_fs_files subdirectory
        }

    let walk_fs_file_contents (directory: string) : (string * string) seq =
        walk_fs_files(directory)
        |> Seq.map (fun file_path -> file_path, File.ReadAllText(file_path))

module Shell =

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