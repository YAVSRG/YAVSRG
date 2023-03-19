namespace Interlude

open System
open System.Reflection
open System.Diagnostics
open System.IO
open Percyqaz.Common
open Prelude.Common

module Utils =

    let getInterludeLocation() = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName)

    /// Numeric version e.g. "0.5.16"
    let smallVersion =
        let v = Assembly.GetExecutingAssembly().GetName()
        if v.Version.Revision <> 0 then v.Version.ToString(4) else v.Version.ToString(3)

    /// Full version string e.g. "Interlude 0.5.16 (20220722)"
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()
        let v2 = Path.Combine(getInterludeLocation(), "Interlude.exe") |> FileVersionInfo.GetVersionInfo
        if DEV_MODE then
            sprintf "%s %s (%s Dev)" v.Name smallVersion v2.ProductVersion
        else
            sprintf "%s %s (%s)" v.Name smallVersion v2.ProductVersion

    /// K for Konst/Kestrel -- K x is shorthand for a function that ignores its input and returns x
    /// Named after the FP combinator
    let K x _ = x

    /// F for Fork -- Combinator to fork two actions. Returns a function that executes them both on the input
    let F (f: 'T -> unit) (g: 'T -> unit) = fun t -> f t; g t

    /// L for localise -- Shorthand to get the localised text from a locale string id
    let L = Localisation.localise

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

    let openUrl (url: string) =
        try Process.Start (ProcessStartInfo (url, UseShellExecute=true)) |> ignore
        with err -> Logging.Debug ("Failed to open url in browser: " + url, err)

    module AutoUpdate =
        open System.IO.Compression
        open Percyqaz.Json
        open Prelude.Data

        // this doesn't just copy a folder to a destination, but renames any existing/duplicates of the same name to .old
        let rec private copyFolder source dest =
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

        let mutable latestVersionName = "<Unknown, server could not be reached>"
        let mutable latestRelease = None
        let mutable updateAvailable = false
        let mutable updateDownloaded = false

        let private handleUpdate(release: GithubRelease) =
            latestRelease <- Some release

            let parseVer (s: string) =
                let s = s.Split(".")
                if s.Length > 3 then (int s.[0], int s.[1], int s.[2], int s.[3])
                else (int s.[0], int s.[1], int s.[2], 0)

            let current = smallVersion
            let incoming = release.tag_name.Substring(1)
            latestVersionName <- incoming

            let pcurrent = parseVer current
            let pincoming = parseVer incoming

            if pincoming > pcurrent then Logging.Info(sprintf "Update available (%s)!" incoming); updateAvailable <- true
            elif pincoming < pcurrent then Logging.Debug(sprintf "Current build (%s) is ahead of update stream (%s)." current incoming)
            else Logging.Info "Game is up to date."

        let checkForUpdates() =
            WebServices.download_json("https://api.github.com/repos/YAVSRG/Interlude/releases/latest", fun (d: GithubRelease option) -> if d.IsSome then handleUpdate d.Value)

            let path = getInterludeLocation()
            let folderPath = Path.Combine(path, "update")
            if Directory.Exists folderPath then Directory.Delete(folderPath, true)

        let applyUpdate(callback) =
            if not updateAvailable then failwith "No update available to install"
            if updateDownloaded then () else

            updateDownloaded <- true

            let download_url = latestRelease.Value.assets.Head.browser_download_url
            let path = getInterludeLocation()
            let zipPath = Path.Combine(path, "update.zip")
            let folderPath = Path.Combine(path, "update")
            File.Delete zipPath
            if Directory.Exists folderPath then Directory.Delete(folderPath, true)
            WebServices.download_file.Request((download_url, zipPath, ignore),
                fun success ->
                    if success then
                        ZipFile.ExtractToDirectory(zipPath, folderPath)
                        File.Delete zipPath
                        copyFolder folderPath path
                        callback()
            )

    type Drawing.Color with
        static member FromHsv(H: float32, S: float32, V: float32) =
            let C = V * S
            let X = C * (1.0f - MathF.Abs((H * 6.0f) %% 2.0f - 1.0f))
            let m = V - C

            let (r, g, b) =
                if H < 1.0f/6.0f then
                    (C, X, 0.0f)
                elif H < 2.0f/6.0f then
                    (X, C, 0.0f)
                elif H < 3.0f/6.0f then
                    (0.0f, C, X)
                elif H < 4.0f/6.0f then
                    (0.0f, X, C)
                elif H < 5.0f/6.0f then
                    (X, 0.0f, C)
                else
                    (C, 0.0f, X)
            Color.FromArgb((r + m) * 255.0f |> int, (g + m) * 255.0f |> int, (b + m) * 255.0f |> int)
            
        /// Doesn't include alpha
        member this.ToHsv() : float32 * float32 * float32 =
            let R = float32 this.R / 255.0f
            let G = float32 this.G / 255.0f
            let B = float32 this.B / 255.0f
            let Cmax = max R G |> max B
            let Cmin = min R G |> min B
            let d = Cmax - Cmin

            let H =
                if d = 0.0f then 0.0f
                elif Cmax = R then
                    (((G - B) / d) %% 6.0f) / 6.0f
                elif Cmax = G then
                    (((B - R) / d) + 2.0f) / 6.0f
                else
                    (((R - G) / d) + 4.0f) / 6.0f

            let S =
                if Cmax = 0.0f then 0.0f
                else d / Cmax

            let V = Cmax
            
            (H, S, V)

        member this.ToHex() : string =
            if this.A = 255uy then
                sprintf "#%02x%02x%02x" this.R this.G this.B
            else sprintf "#%02x%02x%02x%02x" this.R this.G this.B this.A

        static member FromHex(s: string) : Color =
            if s.Length = 9 && s.[0] = '#' then
                let alpha = Convert.ToByte(s.Substring(7), 16)
                Color.FromArgb(int alpha, Drawing.ColorTranslator.FromHtml(s.Substring(0, 7)))
            elif s.Length = 7 && s.[0] = '#' then Drawing.ColorTranslator.FromHtml(s)
            elif s.Length > 0 && s.[0] <> '#' then Drawing.ColorTranslator.FromHtml(s)
            else failwithf "Invalid color code: %s" s