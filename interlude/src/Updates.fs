namespace Interlude

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open Percyqaz.Common
open System.IO.Compression
open Percyqaz.Data
open Prelude
open Prelude.Data

module Updates =

    /// Numeric version e.g. "0.5.16"
    let short_version : string =
        let v = Assembly.GetExecutingAssembly().GetName()

        if v.Version.Revision <> 0 then
            v.Version.ToString(4)
        else
            v.Version.ToString(3)

    /// Github commit SHA
    let short_hash : string =
        let informational_version =
            Assembly
                .GetExecutingAssembly()
                .GetCustomAttribute<AssemblyInformationalVersionAttribute>()
                .InformationalVersion

        let hash = informational_version.Substring(informational_version.IndexOf('+') + 1)
        hash.Substring(0, min hash.Length 6)

    /// Full version string e.g. "Interlude 0.5.16"
    let version : string =
        let v = Assembly.GetExecutingAssembly().GetName()

        if DEV_MODE then
            sprintf "%s %s (%s)" v.Name short_version short_hash
        else
            sprintf "%s %s" v.Name short_version

    let private get_interlude_location () : string =
        Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName)

    // this doesn't just copy a folder to a destination, but renames any existing/duplicates of the same name to .old
    let rec private swap_update_files (source: string) (dest: string) : unit =
        Directory.EnumerateFiles source
        |> Seq.iter (fun source ->
            let target = Path.Combine(dest, Path.GetFileName source)

            try
                File.Move(target, source + ".old", true)
            with
            | :? FileNotFoundException -> ()
            | other -> Logging.Error "Error while moving file '%s' during auto-update: %O" source other
            File.Copy(source, target, true)
        )

        Directory.EnumerateDirectories source
        |> Seq.iter (fun d ->
            let targetd = Path.Combine(dest, Path.GetFileName d)
            Directory.CreateDirectory targetd |> ignore
            swap_update_files d targetd
        )

    [<Json.AutoCodec>]
    type GithubAsset =
        {
            name: string
            browser_download_url: string
        }

    [<Json.AutoCodec>]
    type GithubRelease =
        {
            url: string
            tag_name: string
            name: string
            published_at: string
            body: string
            assets: GithubAsset list
        }

    let mutable restart_on_exit = false

    let mutable latest_version_name = "<Unknown, server could not be reached>"
    let mutable latest_release = None
    let mutable update_available = false
    let mutable update_started = false
    let mutable update_complete = false

    let asset_name =
        match RuntimeInformation.OSArchitecture with
        | Architecture.X64 when OperatingSystem.IsWindows() -> Ok "interlude-win64.zip"
        | Architecture.X64 when OperatingSystem.IsLinux() -> Ok "interlude-linux-x64.zip"
        | other -> Error other

    let private handle_update (release: GithubRelease) : unit =
        latest_release <- Some release

        let parse_version (s: string) =
            let s = s.Split(".")

            if s.Length > 3 then
                (int s.[0], int s.[1], int s.[2], int s.[3])
            else
                (int s.[0], int s.[1], int s.[2], 0)

        let current = short_version
        let incoming = release.tag_name.Replace("interlude-", "").Substring(1)
        latest_version_name <- incoming

        let pcurrent = parse_version current
        let pincoming = parse_version incoming

        match asset_name with
        | Error arch -> Logging.Info "Auto-updater doesn't support this OS or architecture (%O)" arch
        | Ok _ ->

        if pincoming > pcurrent then
            Logging.Info "Update available (%s)!" incoming
            update_available <- true
        elif pincoming < pcurrent then
            Logging.Debug "Current build (%s) is ahead of update stream (%s)." current incoming
        else
            Logging.Info "Game is up to date."

    let check_for_updates () : unit =
        WebServices.download_json (
            "https://api.github.com/repos/YAVSRG/YAVSRG/releases/latest",
            function
            | WebResult.Ok(d: GithubRelease) -> handle_update d
            | _ -> ()
        )

        let path = get_interlude_location ()
        let folder_path = Path.Combine(path, "update")

        if Directory.Exists folder_path then
            Directory.Delete(folder_path, true)

    let apply_update (progress: float32 -> unit, callback: unit -> unit) : unit =
        if not update_available then
            failwith "No update available to install"

        if update_started then
            ()
        else

            update_started <- true

            match
                latest_release.Value.assets
                |> List.tryFind (fun asset -> Ok (asset.name.ToLower()) = asset_name)
            with
            | None ->
                Logging.Error(
                    "Update failed: The github release doesn't have a download for your platform. Report this as a bug!"
                )
            | Some asset ->

            let download_url = asset.browser_download_url
            let path = get_interlude_location ()
            let zip_path = Path.Combine(path, "update.zip")
            let folder_path = Path.Combine(path, "update")
            File.Delete zip_path

            if Directory.Exists folder_path then
                Directory.Delete(folder_path, true)

            WebServices.download_file.Request(
                (download_url, zip_path, progress),
                fun success ->
                    if success then
                        ZipFile.ExtractToDirectory(zip_path, folder_path)
                        File.Delete zip_path
                        swap_update_files folder_path path
                        callback ()
                        update_complete <- true
            )