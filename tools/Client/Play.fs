namespace YAVSRG.CLI.Features

open System.IO
open System.IO.Compression
open YAVSRG.CLI.Utils

module Play =

    let debug_run () =
        exec_at INTERLUDE_SOURCE_PATH "dotnet" "build --configuration Debug -v q"
        exec_at (Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Debug", "net8.0")) "dotnet" "run --project ../../.."

    let GAME_FOLDER = Path.Combine(YAVSRG_PATH, "GAME")

    let update () =
        exec "git" "checkout main"
        exec "git" "pull"
        exec "git" "fetch --tags"

        let tag_digest = eval "git" "rev-list --tags --max-count=1"
        let tag_name = eval "git" (sprintf "describe --tags \"%s\"" tag_digest)
        exec "git" (sprintf "checkout %s" tag_name)

        try
            Directory.CreateDirectory GAME_FOLDER |> ignore

            if System.OperatingSystem.IsWindows() then 
                Releases.build_win_x64()
                ZipFile.ExtractToDirectory(Path.Combine(YAVSRG_PATH, "interlude", "releases", "Interlude-win64.zip"), GAME_FOLDER, true)
            elif System.OperatingSystem.IsLinux() then
                Releases.build_linux_x64()
                ZipFile.ExtractToDirectory(Path.Combine(YAVSRG_PATH, "interlude", "releases", "Interlude-linux-x64.zip"), GAME_FOLDER, true)
            elif System.OperatingSystem.IsMacOS() then 
                Releases.build_osx_arm64()
                ZipFile.ExtractToDirectory(Path.Combine(YAVSRG_PATH, "interlude", "releases", "Interlude-osx-arm64.zip"), GAME_FOLDER, true)
            else printfn "Your platform is not supported! Maybe complain in the discord?"

        with err -> printfn "Error creating GAME folder: %O" err

        exec "git" "checkout main"

    let play () =

        if not (Directory.Exists GAME_FOLDER && Directory.EnumerateFileSystemEntries GAME_FOLDER |> Seq.isEmpty |> not) then
            update()

        if File.Exists(Path.Combine(GAME_FOLDER, "Interlude.exe")) then
            exec_at GAME_FOLDER "Interlude.exe" ""
        elif File.Exists(Path.Combine(GAME_FOLDER, "Interlude")) then
            exec_at GAME_FOLDER "Interlude" ""
        else
            printfn "Your GAME folder is missing an Interlude executable, run `yavsrg update` to fix it"