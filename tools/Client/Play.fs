namespace YAVSRG.CLI

open System.IO
open System.Runtime.InteropServices

module Play =

    let detect_build_info () =
        let arch = RuntimeInformation.OSArchitecture

        match arch with
        | Architecture.X64 when System.OperatingSystem.IsWindows() -> Releases.BuildPlatformInfo.WINDOWS_X64
        | Architecture.X64 when System.OperatingSystem.IsLinux() -> Releases.BuildPlatformInfo.LINUX_X64
        | Architecture.X64 when System.OperatingSystem.IsMacOS() -> Releases.BuildPlatformInfo.OSX_X64
        | Architecture.Arm64 when System.OperatingSystem.IsMacOS() -> Releases.BuildPlatformInfo.OSX_ARM64
        | _ -> failwithf "Your platform (%O) is not supported! Maybe complain in the discord?" arch

    let debug_run () : unit =
        let build_output =
            Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Debug", DOTNET_VERSION)

        try
            let build_info = detect_build_info()

            if not(Directory.Exists(build_output)) then
                Directory.CreateDirectory(build_output) |> ignore

            File.Copy(
                Path.Combine(YAVSRG_PATH, "engine", "lib", build_info.RuntimeId, build_info.BassLibraryFile),
                Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Debug", DOTNET_VERSION, build_info.BassLibraryFile),
                true
            )

            File.Copy(
                Path.Combine(YAVSRG_PATH, "engine", "lib", build_info.RuntimeId, build_info.BassFxLibraryFile),
                Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Debug", DOTNET_VERSION, build_info.BassFxLibraryFile),
                true
            )
        with err ->
            printfn "Error detecting platform: %O" err

        Shell.At(INTERLUDE_SOURCE_PATH).Exec("dotnet", "run --configuration Debug -v q") |> ignore
