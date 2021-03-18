namespace Interlude.UI

open System
open System.IO
open System.Drawing
open Percyqaz.Json
open Prelude.Common
open Prelude.Data.ChartManager.Sorting
open Prelude.Web
open Interlude
open Interlude.Utils
open Interlude.UI.Components

module FileDropHandling =
    let import(path: string) =
        BackgroundTask.Create(TaskFlags.NONE)("Import " + Path.GetFileName(path))
            (Gameplay.cache.AutoConvert(path) |> BackgroundTask.Callback(fun b -> ScreenLevelSelect.refresh <- ScreenLevelSelect.refresh || b))
        |> ignore

module ScreenImport =

    [<Json.AllRequired>]
    type EOPackAttrs = {
        name: string
        average: float
        download: string
        size: int64
    } with static member Default = { name = ""; average = 0.0; download = ""; size = 0L }

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
    } with static member Default = { difficulty = 0.0; difficulty_cs = 0.0; bpm = 0; play_length = 0; mapper = ""; artist = ""; title = ""; beatmapset_id = 0 }
    
    [<Json.AllRequired>]
    type BeatmapSearch = {
        result_count: int
        beatmaps: ResizeArray<BeatmapData>
    } with static member Default = { result_count = -1; beatmaps = null }

    type SMImportCard(data: EOPackAttrs) as this =
        inherit Frame()
        let mutable downloaded = false //todo: maybe check if pack is already installed?
        let download() =
            let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".zip")
            Screens.addNotification(Localisation.localiseWith [data.name] "notification.PackDownloading", NotificationType.Task)
            BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.name)
                (BackgroundTask.Chain
                    [
                        downloadFile(data.download, target)
                        (Gameplay.cache.AutoConvert(target)
                            |> BackgroundTask.Callback(fun b -> ScreenLevelSelect.refresh <- ScreenLevelSelect.refresh || b; Screens.addNotification(Localisation.localiseWith [data.name] "notification.PackInstalled", NotificationType.Task); File.Delete(target)))
                    ]) |> ignore
            downloaded <- true
        do
            this.Add(new TextBox(K data.name, K (Color.White, Color.Black), 0.5f))
            //size in mb, average difficulty
            this.Add(new Clickable((fun () -> if not downloaded then download()), ignore))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f)
        member this.Data = data
        static member Filter(filter: Filter) =
            fun (c: Widget) ->
                let c = c :?> SMImportCard
                List.forall (
                    function
                    | Impossible -> false
                    | String str -> c.Data.name.ToLower().Contains(str)
                    | _ -> true
                ) filter

    type BeatmapImportCard(data: BeatmapData) as this =
        inherit Frame()
        let mutable downloaded = false
        let download() =
            let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".osz")
            Screens.addNotification(Localisation.localiseWith [data.title] "notification.SongDownloading", NotificationType.Task)
            BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.title)
                (BackgroundTask.Chain
                    [
                        downloadFile(sprintf "http://beatconnect.io/b/%i/" data.beatmapset_id, target)
                        (Gameplay.cache.AutoConvert(target)
                            |> BackgroundTask.Callback(fun b -> ScreenLevelSelect.refresh <- ScreenLevelSelect.refresh || b; Screens.addNotification(Localisation.localiseWith [data.title] "notification.SongInstalled", NotificationType.Task); File.Delete(target)))
                    ]) |> ignore
            downloaded <- true
        do
            this.Add(new TextBox(K (data.artist + " - " + data.title), K (Color.White, Color.Black), 0.0f))
            this.Add(new Clickable((fun () -> if not downloaded then download()), ignore))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f)

    type SearchContainerLoader(t: StatusTask) as this =
        inherit Widget()
        let mutable task = None
        do
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 20.0f, 0.0f)
        //loader is only drawn if it is visible
        override this.Draw() =
            base.Draw()
            //draw little loading indicator here
            Interlude.Render.Draw.rect this.Bounds Color.Fuchsia Interlude.Render.Sprite.Default
            if task.IsNone then task <- Some <| BackgroundTask.Create TaskFlags.HIDDEN "Search container loading" (t |> BackgroundTask.Callback(fun _ -> this.RemoveFromParent()))
        override this.Dispose() = match task with None -> () | Some task -> task.Cancel()

    type SearchContainer(populate, handleFilter) as this =
        inherit Widget()
        let flowContainer = new FlowContainer()
        let populate = populate flowContainer
        let handleFilter = handleFilter flowContainer
        do
            this.Add(new SearchBox(new Setting<string>(""), fun f -> handleFilter f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f))
            this.Add(flowContainer |> positionWidget(0.0f, 0.0f, 60.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f))
            flowContainer.Add(new SearchContainerLoader(populate))
    
    let rec beatmapSearch (filter: Filter) (page: int) : FlowContainer -> StatusTask =
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
            | Criterion ("keys", n) -> match Int32.TryParse(n) with (true, i) -> s <- s + sprintf "&cs=(%i.0, %i.0)" i i | _ -> ()
            | Criterion ("m", m)
            | Criterion ("c", m)
            | Criterion ("creator", m)
            | Criterion ("mapper", m) -> s <- s + "&mapper=" + m
            | _ -> ()
        ) filter
        s <- s + "&title=" + Uri.EscapeDataString title
        s <- s + "&offset=" + page.ToString()
        fun (flowContainer: FlowContainer) output ->
            let callback(d: BeatmapSearch) =
                for p in d.beatmaps do flowContainer.Add(new BeatmapImportCard(p))
                if d.result_count < 0 || d.result_count > d.beatmaps.Count then
                    flowContainer.Add(new SearchContainerLoader(beatmapSearch filter (page + 1) flowContainer))
            downloadJson(s, callback)

open ScreenImport

type ScreenImport() as this =
    inherit Screen()
    do
        let eoDownloads = 
            SearchContainer(
                (fun flowContainer output -> downloadJson("https://api.etternaonline.com/v2/packs/", (fun (d: {| data: ResizeArray<EOPack> |}) -> for p in d.data do flowContainer.Add(new SMImportCard(p.attributes)) ))),
                (fun flowContainer filter -> flowContainer.Filter(SMImportCard.Filter filter)) )
        let osuDownloads =
            SearchContainer(
                (beatmapSearch [] 0),
                (fun flowContainer filter -> flowContainer.Clear(); flowContainer.Add(new SearchContainerLoader(beatmapSearch filter 0 flowContainer))) )
        let tabs = new TabContainer("EtternaOnline", eoDownloads)
        tabs.AddTab("osu!", osuDownloads)
        this.Add(tabs |> positionWidget(600.0f, 0.0f, 50.0f, 0.0f, -100.0f, 1.0f, -50.0f, 1.0f))