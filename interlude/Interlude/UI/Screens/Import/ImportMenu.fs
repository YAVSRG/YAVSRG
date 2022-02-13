namespace Interlude.UI.Screens.Import

open System
open System.IO
open System.Net
open System.Net.Security
open Percyqaz.Json
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Web
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Screens.LevelSelect

module FileDropHandling =
    let tryImport(path: string) : bool =
        match Mounts.dropFunc with
        | Some f -> f path; true
        | None ->
            BackgroundTask.Create TaskFlags.NONE ("Import " + Path.GetFileName path)
                (Library.Imports.autoConvert path |> BackgroundTask.Callback(fun b -> LevelSelect.refresh <- LevelSelect.refresh || b))
            |> ignore
            true

[<Json.AllRequired>]
type EOPackAttrs = {
    name: string
    average: float
    download: string
    mirror: string
    size: int64
} with static member Default = { name = ""; average = 0.0; download = ""; mirror = ""; size = 0L }

[<Json.AllRequired>]
type EOPack = {
    ``type``: string
    id: int
    attributes: EOPackAttrs
} with static member Default = {``type`` = "pack"; id = 0; attributes = EOPackAttrs.Default }

[<Json.AllRequired>]
type BeatmapData = {
    difficulty_cs: float //key count
    difficulty: float
    bpm: int
    play_length: int
    mapper: string
    artist: string
    title: string
    beatmapset_id: int
    beatmap_status: int
} with static member Default = { difficulty = 0.0; difficulty_cs = 0.0; bpm = 0; play_length = 0; mapper = ""; artist = ""; title = ""; beatmapset_id = 0; beatmap_status = 0 }
    
[<Json.AllRequired>]
type BeatmapSearch = {
    result_count: int
    beatmaps: ResizeArray<BeatmapData>
} with static member Default = { result_count = -1; beatmaps = null }

type private SMImportCard(data: EOPackAttrs) as this =
    inherit Frame((fun () -> Style.accentShade(120, 1.0f, 0.0f)), (fun () -> Style.accentShade(200, 1.0f, 0.2f)))
    let mutable downloaded = false //todo: maybe check if pack is already installed?
    let download() =
        let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".zip")
        Notification.add (Localisation.localiseWith [data.name] "notification.download.pack", NotificationType.Task)
        BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.name)
            (BackgroundTask.Chain
                [
                    downloadFile(data.download, target)
                    (Library.Imports.autoConvert target
                        |> BackgroundTask.Callback( fun b -> 
                            LevelSelect.refresh <- LevelSelect.refresh || b
                            Notification.add (Localisation.localiseWith [data.name] "notification.install.pack", NotificationType.Task)
                            File.Delete target ))
                ]) |> ignore
        downloaded <- true
    do
        this.Add(new TextBox(K data.name, K (Color.White, Color.Black), 0.0f))
        this.Add(new TextBox(K (sprintf "Difficulty: %.2f   %.1fMB" data.average (float data.size / 1000000.0)), K (Color.White, Color.Black), 1.0f))
        this.Add(new Clickable((fun () -> if not downloaded then download()), ignore))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f)

    member this.Data = data

    static member Filter(filter: Filter) =
        fun (c: Widget) ->
            match c with
            | :? SMImportCard as c ->
                List.forall (
                    function
                    | Impossible -> false
                    | String str -> c.Data.name.ToLower().Contains(str)
                    | _ -> true
                ) filter
            | _ -> true

type private BeatmapImportCard(data: BeatmapData) as this =
    inherit Widget()
            
    let mutable downloaded = false
    let download() =
        let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".osz")
        Notification.add (Localisation.localiseWith [data.title] "notification.download.song", NotificationType.Task)
        BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.title)
            (BackgroundTask.Chain
                [
                    downloadFile(sprintf "http://beatconnect.io/b/%i/" data.beatmapset_id, target)
                    (Library.Imports.autoConvert target
                    |> BackgroundTask.Callback( fun b -> 
                        LevelSelect.refresh <- LevelSelect.refresh || b
                        Notification.add (Localisation.localiseWith [data.title] "notification.install.song", NotificationType.Task)
                        File.Delete target ))
                ]) |> ignore
        downloaded <- true
    do
        let c =
            match data.beatmap_status with
            | 1 -> Color.Aqua //ranked
            | 3 -> Color.Lime //qualified
            | 4 -> Color.HotPink //loved
            | 0 -> Color.LightGoldenrodYellow //pending
            | -1 -> Color.White //wip
            | -2 //graveyard
            | _ -> Color.Gray
        this.Add(new Frame(Color.FromArgb(120, c), Color.FromArgb(200, c)))
        this.Add(new TextBox(K (data.artist + " - " + data.title), K (Color.White, Color.Black), 0.0f)
            |> positionWidgetA(0.0f, 0.0f, -400.0f, -30.0f))
        this.Add(new TextBox(K ("Created by " + data.mapper), K (Color.White, Color.Black), 0.0f)
            |> positionWidgetA(0.0f, 40.0f, 0.0f, 0.0f))
        this.Add(new TextBox(K (sprintf "%.2f*   %iBPM   %iK" data.difficulty data.bpm (int data.difficulty_cs)), K (Color.White, Color.Black), 1.0f)
            |> positionWidgetA(0.0f, 20.0f, 0.0f, -20.0f))
        this.Add(new Clickable((fun () -> if not downloaded then download()), ignore))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f)

type private SearchContainerLoader(t) as this =
    inherit Widget()
    let t = t this
    let mutable task = None
    do this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 30.0f, 0.0f)

    //loader is only drawn if it is visible on screen
    override this.Draw() =
        base.Draw()
        //todo: improved loading indicator here
        Text.drawFill(Content.font, "Loading...", this.Bounds, Color.White, 0.5f)
        if task.IsNone then task <- Some <| BackgroundTask.Create TaskFlags.HIDDEN "Search container loading" (t |> BackgroundTask.Callback(fun _ -> if this.Parent.IsSome then this.Destroy()))

type private SearchContainer(populate, handleFilter) as this =
    inherit Widget()
    let flowContainer = new FlowContainer(Spacing = 15.0f)
    let populate = populate flowContainer
    let handleFilter = handleFilter flowContainer
    do
        this.Add(new SearchBox(Setting.simple "", fun f -> handleFilter f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f))
        this.Add(flowContainer |> positionWidget(0.0f, 0.0f, 70.0f, 0.0f, -0.0f, 1.0f, 0.0f, 1.0f))
        flowContainer.Add(new SearchContainerLoader(populate))

module private Beatmap =
    
    let rec search (filter: Filter) (page: int) : FlowContainer -> SearchContainerLoader -> StatusTask =
        let mutable s = "https://osusearch.com/api/search?modes=Mania&key="
        let mutable invalid = false
        let mutable title = ""
        List.iter(
            function
            | Impossible -> invalid <- true
            | String "-p" -> s <- s + "&premium_mappers=true"
            | String s -> match title with "" -> title <- s | t -> title <- t + " " + s
            | Criterion ("k", n)
            | Criterion ("key", n)
            | Criterion ("keys", n) -> match Int32.TryParse n with (true, i) -> s <- s + sprintf "&cs=(%i.0, %i.0)" i i | _ -> ()
            | Criterion ("m", m)
            | Criterion ("c", m)
            | Criterion ("creator", m)
            | Criterion ("mapper", m) -> s <- s + "&mapper=" + m
            | _ -> ()
        ) filter
        s <- s + "&title=" + Uri.EscapeDataString title
        s <- s + "&offset=" + page.ToString()
        fun (flowContainer: FlowContainer) (loader: SearchContainerLoader) output ->
            let callback(d: BeatmapSearch) =
                if loader.Parent.IsSome then
                    flowContainer.Synchronized(
                        fun () -> 
                            for p in d.beatmaps do flowContainer.Add(BeatmapImportCard p)
                            if d.result_count < 0 || d.result_count > d.beatmaps.Count then
                                new SearchContainerLoader(search filter (page + 1) flowContainer)
                                |> flowContainer.Add
                    )
            downloadJson(s, callback)

type Screen() as this =
    inherit Screen.T()
    do
        (*
            Online downloaders
        *)

        // EtternaOnline's certificate keeps expiring!! Rop get on it
        // This hack force-trusts EO's SSL certificate even though it has expired (this was for 18th march 2021, there's a new working certificate currently)
        ServicePointManager.ServerCertificateValidationCallback <-
            RemoteCertificateValidationCallback(
                fun _ cert _ sslPolicyErrors ->
                    if sslPolicyErrors = SslPolicyErrors.None then true
                    else cert.GetCertHashString().ToLower() = "a4deab1b3cbd43ac7d21c9d17b80d97df09e67ee" )

        let eoDownloads = 
            SearchContainer(
                (fun flowContainer _ output -> downloadJson("https://api.etternaonline.com/v2/packs/", (fun (d: {| data: ResizeArray<EOPack> |}) -> flowContainer.Synchronized(fun () -> for p in d.data do flowContainer.Add(new SMImportCard(p.attributes))) ))),
                (fun flowContainer filter -> flowContainer.Filter <- SMImportCard.Filter filter) )
        let osuDownloads =
            SearchContainer(
                (Beatmap.search [] 0),
                (fun flowContainer filter -> flowContainer.Clear(); flowContainer.Add(new SearchContainerLoader(Beatmap.search filter 0 flowContainer))) )
        let tabs = new TabContainer("Etterna Packs", eoDownloads)
        tabs.AddTab("osu! Songs", osuDownloads)

        tabs
        |> positionWidget(600.0f, 0.0f, 50.0f, 0.0f, -100.0f, 1.0f, -80.0f, 1.0f)
        |> this.Add

        new TextBox(K "(Interlude is not affiliated with osu! or Etterna, these downloads are provided through unofficial APIs)", K (Color.White, Color.Black), 0.5f)
        |> positionWidget(600.0f, 0.0f, -90.0f, 1.0f, -100.0f, 1.0f, -30.0f, 1.0f)
        |> this.Add

        (*
            Offline importers from other games
        *)

        MountControl(Mounts.Types.Osu, Options.options.OsuMount)
        |> positionWidget(0.0f, 0.0f, 200.0f, 0.0f, 360.0f, 0.0f, 260.0f, 0.0f)
        |> this.Add

        MountControl(Mounts.Types.Stepmania, Options.options.StepmaniaMount)
        |> positionWidget(0.0f, 0.0f, 270.0f, 0.0f, 360.0f, 0.0f, 330.0f, 0.0f)
        |> this.Add

        MountControl(Mounts.Types.Etterna, Options.options.EtternaMount)
        |> positionWidget(0.0f, 0.0f, 340.0f, 0.0f, 360.0f, 0.0f, 400.0f, 0.0f)
        |> this.Add

        new TextBox(K "Import from game", K (Color.White, Color.Black), 0.5f )
        |> positionWidget(0.0f, 0.0f, 150.0f, 0.0f, 250.0f, 0.0f, 200.0f, 0.0f)
        |> this.Add

    override this.OnEnter _ = ()
    override this.OnExit _ = ()