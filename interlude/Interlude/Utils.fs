namespace Interlude

open System.Reflection
open System.Diagnostics
open System.IO

module Utils =
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()
        let v2 = Assembly.GetExecutingAssembly().Location |> FileVersionInfo.GetVersionInfo
        sprintf "%s %s (%s)" v.Name (v.Version.ToString(3)) v2.ProductVersion

    let K x _ = x

    let getResourceStream name =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources." + name)

    let randomSplash(name) =
        let r = new System.Random()
        use s = getResourceStream(name)
        use tr = new StreamReader(s)
        let lines = tr.ReadToEnd().Split("\n")

        fun () -> lines.[r.Next(lines.Length)]

    let idPrint x = printfn "%A" x; x

    module AutoUpdate =
        open Percyqaz.Json
        open Prelude.Common
        open Prelude.Web

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

        let handleUpdate(release: GithubRelease) =
            let current = Assembly.GetExecutingAssembly().GetName().Version.ToString(3)
            let incoming = release.tag_name.Substring(1)
            if incoming > current then
                Logging.Info(sprintf "Update available (%s)!" incoming)""
            elif incoming < current then
                Logging.Debug(sprintf "Current build (%s) is ahead of update stream (%s)." current incoming)""
            else
                Logging.Info("Game is up to date")""

        let checkForUpdates() =
            BackgroundTask.Create TaskFlags.HIDDEN "Checking for updates"
                (fun output -> downloadJson("https://api.github.com/repos/percyqaz/YAVSRG/releases/latest", (fun (d: GithubRelease) -> handleUpdate(d))))