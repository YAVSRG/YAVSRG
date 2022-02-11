namespace Interlude

open System
open System.Reflection
open System.Diagnostics
open System.IO
open Prelude.Common

module Utils =
    let smallVersion =
        let v = Assembly.GetExecutingAssembly().GetName()
        if v.Version.Revision <> 0 then v.Version.ToString(4) else v.Version.ToString(3)
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()
        let v2 = Assembly.GetExecutingAssembly().Location |> FileVersionInfo.GetVersionInfo
        #if DEBUG
        sprintf "%s %s (%s Dev)" v.Name smallVersion v2.ProductVersion
        #else
        sprintf "%s %s (%s)" v.Name smallVersion v2.ProductVersion
        #endif

    let K x _ = x

    let F (f: 'T -> unit) (g: 'T -> unit) = fun t -> f t; g t

    let L = Localisation.localise

    let getInterludeLocation() = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

    let getResourceStream name =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources." + name)

    let getResourceText name =
        use s = getResourceStream name
        use tr = new StreamReader(s)
        tr.ReadToEnd()

    let randomSplash(name) =
        let r = new Random()
        let text = getResourceText name
        let lines = text.Split("\n")

        fun () -> lines.[r.Next lines.Length]

    let idPrint x = printfn "%A" x; x

    let formatTimeOffset (ts: TimeSpan) =
       if ts.TotalDays > 365.0 then sprintf "%.0fy" (ts.TotalDays / 365.0)
       elif ts.TotalDays > 30.0 then sprintf "%.0fmo" (ts.TotalDays / 30.0)
       elif ts.TotalDays > 7.0 then sprintf "%.0fw" (ts.TotalDays / 7.0)
       elif ts.TotalDays > 1.0 then sprintf "%.0fd" ts.TotalDays
       elif ts.TotalHours > 1.0 then sprintf "%.0fh" ts.TotalHours
       elif ts.TotalMinutes > 1.0 then sprintf "%.0fm" ts.TotalMinutes
       else sprintf "%.0fs" ts.TotalSeconds

    let openDirectory (path: string) =
        ProcessStartInfo("file://" + System.IO.Path.GetFullPath path, UseShellExecute = true)
        |> Process.Start
        |> ignore

    module AutoUpdate =
        open System.IO.Compression
        open Percyqaz.Json
        open Prelude.Web

        // this doesn't just copy a folder to a destination, but renames any existing/duplicates of the same name to .old
        let rec copyFolder source dest =
            Directory.EnumerateFiles source
            |> Seq.iter
                (fun s ->
                    let fDest = Path.Combine(dest, Path.GetFileName s)
                    try File.Copy(s, fDest, true)
                    with :? IOException as err ->
                        File.Move(fDest, s + ".old", true)
                        File.Copy(s, fDest, true)
                )
            Directory.EnumerateDirectories source
            |> Seq.iter
                ( fun d ->
                    let targetd = Path.Combine(dest, Path.GetFileName d)
                    Directory.CreateDirectory targetd |> ignore
                    copyFolder d targetd
                )

        [<Json.AllRequired>]
        type GithubAsset = {
            name: string
            browser_download_url: string
        } with static member Default = { name = ""; browser_download_url = "" }
        
        [<Json.AllRequired>]
        type GithubRelease = {
            url: string
            tag_name: string
            name: string
            published_at: string
            body: string
            assets: GithubAsset list
        } with static member Default = { name = ""; url = ""; tag_name = ""; published_at = ""; body = ""; assets = [] }

        let mutable latestRelease = None
        let mutable updateAvailable = false

        let handleUpdate(release: GithubRelease) =
            latestRelease <- Some release

            let parseVer (s: string) =
                let s = s.Split(".")
                (int s.[0], int s.[1], int s.[2])

            let current = smallVersion
            let incoming = release.tag_name.Substring(1)

            let pcurrent = parseVer current
            let pincoming = parseVer incoming

            // if parseVer crashes because of some not-well-formed version, so be it. this is not inside the main thread and that error will get logged
            // parseVer ensures 0.4.10 > 0.4.9 where string comparison gives the wrong answer

            if pincoming > pcurrent then Logging.Info(sprintf "Update available (%s)!" incoming); updateAvailable <- true
            elif pincoming < pcurrent then Logging.Debug(sprintf "Current build (%s) is ahead of update stream (%s)." current incoming)
            else Logging.Info "Game is up to date."

        let checkForUpdates() =
            BackgroundTask.Create TaskFlags.HIDDEN "Checking for updates"
                (fun output -> downloadJson("https://api.github.com/repos/percyqaz/YAVSRG/releases/latest", fun (d: GithubRelease) -> handleUpdate d))
            |> ignore

            let path = getInterludeLocation()
            let folderPath = Path.Combine(path, "update")
            if Directory.Exists folderPath then Directory.Delete(folderPath, true)

        // call this only if updateAvailable = true
        let applyUpdate(callback) =
            let download_url = latestRelease.Value.assets.Head.browser_download_url
            let path = getInterludeLocation()
            let zipPath = Path.Combine(path, "update.zip")
            let folderPath = Path.Combine(path, "update")
            File.Delete zipPath
            if Directory.Exists folderPath then Directory.Delete(folderPath, true)
            BackgroundTask.Create TaskFlags.NONE ("Downloading update " + latestRelease.Value.tag_name)
                (downloadFile (download_url, zipPath)
                |> BackgroundTask.Callback
                    (fun b ->
                        if b then
                            ZipFile.ExtractToDirectory(zipPath, folderPath)
                            File.Delete zipPath
                            copyFolder folderPath path
                            callback()
                    ))
            |> ignore

module Icons = 
    let star = Feather.star
    let back = Feather.arrow_left
    let bpm = Feather.music
    let time = Feather.clock
    let sparkle = Feather.award

    let edit = Feather.edit_2
    let add = Feather.plus_circle
    let remove = Feather.trash
    let selected = Feather.check_circle
    let unselected = Feather.circle

    let goal = Feather.flag
    let playlist = Feather.list

    let system = Feather.cast
    let themes = Feather.image
    let gameplay = Feather.sliders
    let binds = Feather.link
    let debug = Feather.terminal

    let info = Feather.info
    let alert = Feather.alert_circle