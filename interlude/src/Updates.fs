namespace Interlude

open System
open System.IO
open System.Diagnostics
open System.Reflection
open Percyqaz.Common
open System.IO.Compression
open Percyqaz.Data
open Prelude
open Prelude.Data

module Updates =

    /// Numeric version e.g. "0.5.16"
    let short_version =
        let v = Assembly.GetExecutingAssembly().GetName()

        if v.Version.Revision <> 0 then
            v.Version.ToString(4)
        else
            v.Version.ToString(3)

    /// Full version string e.g. "Interlude 0.5.16"
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()

        if DEV_MODE then
            sprintf "%s %s (dev build)" v.Name short_version
        else
            sprintf "%s %s" v.Name short_version

    let private get_interlude_location () =
        Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName)

    // this doesn't just copy a folder to a destination, but renames any existing/duplicates of the same name to .old
    let rec private swap_update_files source dest =
        Directory.EnumerateFiles source
        |> Seq.iter (fun source ->
            let target = Path.Combine(dest, Path.GetFileName source)

            try
                File.Copy(source, target, true)
            with :? IOException as err ->
                File.Move(target, source + ".old", true)
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

    let private handle_update (release: GithubRelease) =
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

        if pincoming > pcurrent then
            Logging.Info(sprintf "Update available (%s)!" incoming)
            update_available <- true
        elif pincoming < pcurrent then
            Logging.Debug(sprintf "Current build (%s) is ahead of update stream (%s)." current incoming)
        else
            Logging.Info "Game is up to date."

    let check_for_updates () =
        if OperatingSystem.IsWindows() then
            WebServices.download_json (
                "https://api.github.com/repos/YAVSRG/YAVSRG/releases/latest",
                function
                | WebResult.Ok (d: GithubRelease) -> handle_update d
                | _ -> ()
            )

            let path = get_interlude_location ()
            let folder_path = Path.Combine(path, "update")

            if Directory.Exists folder_path then
                Directory.Delete(folder_path, true)
        else
            Logging.Info "Auto-updater not availble for macOS / Linux (yet)"

    let apply_update (callback) =
        if not update_available then
            failwith "No update available to install"


        if update_started then
            ()
        else

            update_started <- true

            let asset_name =
                // todo: other platforms eventually
                "interlude-win64.zip"

            match latest_release.Value.assets |> List.tryFind (fun asset -> asset.name.ToLower().StartsWith(asset_name)) with
            | None -> Logging.Error("Update failed: The github release doesn't have a download for your platform. Report this to Percyqaz!")
            | Some asset -> 

            let download_url = asset.browser_download_url
            let path = get_interlude_location ()
            let zip_path = Path.Combine(path, "update.zip")
            let folder_path = Path.Combine(path, "update")
            File.Delete zip_path

            if Directory.Exists folder_path then
                Directory.Delete(folder_path, true)

            WebServices.download_file.Request(
                (download_url, zip_path, ignore),
                fun success ->
                    if success then
                        ZipFile.ExtractToDirectory(zip_path, folder_path)
                        File.Delete zip_path
                        swap_update_files folder_path path
                        callback ()
                        update_complete <- true
            )