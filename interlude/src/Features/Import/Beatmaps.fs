namespace Interlude.Features.Import

open System
open System.IO
open Percyqaz.Json
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Web
open Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Features.LevelSelect

type BeatmapStatus =
    | PENDING = 0
    | RANKED = 1
    | QUALIFIED = 3
    | LOVED = 4
    | WORK_IN_PROGRESS = -1
    | GRAVEYARD = -2

[<Json.AutoCodec>]
type BeatmapData =
    {
        difficulty_cs: float //key count
        difficulty: float
        bpm: int
        play_length: int
        mapper: string
        artist: string
        title: string
        beatmapset_id: int
        beatmap_status: BeatmapStatus
    }
    
[<Json.AutoCodec>]
type BeatmapSearch =
    {
        result_count: int
        beatmaps: ResizeArray<BeatmapData>
    }

type private BeatmapImportCard(data: BeatmapData) as this =
    inherit StaticContainer(NodeType.None)
            
    let mutable downloaded = false
    let download() =
        let target = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".osz")
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
            | BeatmapStatus.RANKED -> Color.Aqua
            | BeatmapStatus.QUALIFIED -> Color.Lime
            | BeatmapStatus.LOVED -> Color.HotPink
            | BeatmapStatus.PENDING -> Color.LightGoldenrodYellow
            | BeatmapStatus.WORK_IN_PROGRESS -> Color.White
            | BeatmapStatus.GRAVEYARD
            | _ -> Color.Gray

        this
        |+ Frame(NodeType.None, Fill = K (Color.FromArgb(120, c)), Border = K (Color.FromArgb(200, c)))
        |+ Text(data.artist + " - " + data.title,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 400.0f; Bottom = 1.0f %- 30.0f })
        |+ Text("Created by " + data.mapper,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 40.0f; Right = Position.max; Bottom = Position.max })
        |+ Text(sprintf "%.2f*   %iBPM   %iK" data.difficulty data.bpm (int data.difficulty_cs),
            Align = Alignment.RIGHT,
            Position = { Left = Position.min; Top = 0.0f %+ 20.0f; Right = 1.0f %- 5.0f; Bottom = 1.0f %- 20.0f })
        |* Clickable((fun () -> if not downloaded then download()))

type private SearchContainerLoader(t) as this =
    inherit StaticWidget(NodeType.None)
    let t = t this
    let mutable task = None

    // loader is only drawn if it is visible on screen
    override this.Draw() =
        // todo: improved loading indicator here
        Text.drawFill(Style.baseFont, "Loading...", this.Bounds, Color.White, 0.5f)
        // todo: hide this when complete
        if task.IsNone then task <- Some <| BackgroundTask.Create TaskFlags.HIDDEN "Search container loading" t// |> BackgroundTask.Callback(fun _ -> if this.Parent.IsSome then this.Destroy()))

module private Beatmap =
    
    let rec search (filter: Filter) (page: int) : FlowContainer.Base<_> -> SearchContainerLoader -> StatusTask =
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
        fun (flowContainer: FlowContainer.Base<Widget>) (loader: SearchContainerLoader) output ->
            let callback(d: BeatmapSearch) =
                // todo: this is busted. need to use AsyncManyWorker
                //if loader.Parent.IsSome then
                    sync(
                        fun () -> 
                            for p in d.beatmaps do flowContainer.Add(BeatmapImportCard p)
                            if d.result_count < 0 || d.result_count > d.beatmaps.Count then
                                SearchContainerLoader(search filter (page + 1) flowContainer)
                                |> flowContainer.Add
                    )
            downloadJson(s, callback)