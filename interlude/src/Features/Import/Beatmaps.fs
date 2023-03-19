namespace Interlude.Features.Import

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data
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
    inherit StaticContainer(NodeType.Button(fun () -> this.Download()))
            
    let mutable status = NotDownloaded
    let mutable progress = 0.0f
    let download() =
        if status = NotDownloaded || status = DownloadFailed then
            let target = Path.Combine(getDataPath "Downloads", Guid.NewGuid().ToString() + ".osz")
            WebServices.download_file.Request((sprintf "http://beatconnect.io/b/%i/" data.beatmapset_id, target, fun p -> progress <- p),
                fun completed ->
                    if completed then Library.Imports.auto_convert.Request(target,
                        fun b ->
                            LevelSelect.refresh <- LevelSelect.refresh || b
                            Notifications.task_feedback (Icons.download, L"notification.install_song", data.title)
                            File.Delete target
                            status <- if b then Installed else DownloadFailed
                    )
                    else status <- DownloadFailed
                )
            status <- Downloading

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
        |+ Frame(NodeType.None, Fill = K (Color.FromArgb(120, c)), Border = fun () -> if this.Focused then Color.White else Color.FromArgb(200, c))
        |+ Text(data.artist + " - " + data.title,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 400.0f; Bottom = 1.0f %- 30.0f })
        |+ Text("Created by " + data.mapper,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 40.0f; Right = Position.max; Bottom = Position.max })
        |+ Text(sprintf "%.2f*   %iBPM   %iK" data.difficulty data.bpm (int data.difficulty_cs),
            Align = Alignment.RIGHT,
            Position = Position.TrimRight(160.0f).Margin(5.0f, 20.0f))
        |+ Button(Icons.open_in_browser,
            fun () -> openUrl(sprintf "https://osu.ppy.sh/beatmapsets/%i" data.beatmapset_id)
            ,
            Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f))
        |* Button(Icons.download, download,
            Position = Position.SliceRight(80.0f).Margin(5.0f, 10.0f))

    override this.Draw() =
        base.Draw()

        match status with
        | NotDownloaded -> ()
        | Downloading -> Draw.rect(this.Bounds.SliceLeft(this.Bounds.Width * progress)) (Color.FromArgb(64, 255, 255, 255))
        | Installed -> 
            Draw.rect this.Bounds (Color.FromArgb(64, 255, 255, 255))
            Text.drawFill(Style.baseFont, "Downloaded!", this.Bounds.SliceBottom(25.0f), Color.White, Alignment.CENTER)
        | DownloadFailed ->
            Text.drawFill(Style.baseFont, "Download failed!", this.Bounds.SliceBottom(25.0f), Color.FromArgb(255, 100, 100), Alignment.CENTER)

    member private this.Download() = download()

module Beatmaps =

    let download_json_switch = 
        { new Async.SwitchService<string, BeatmapSearch option>()
            with override this.Handle(url) = WebServices.download_json_async<BeatmapSearch>(url)
        }
    
    let rec search (filter: Filter) (page: int) : PopulateFunc =
        let mutable s = "https://osusearch.com/api/search?modes=Mania&key="
        let mutable invalid = false
        let mutable title = ""
        List.iter(
            function
            | Impossible -> invalid <- true
            | String "#p" -> s <- s + "&premium_mappers=true"
            | String s -> match title with "" -> title <- s | t -> title <- t + " " + s
            | Equals ("k", n)
            | Equals ("key", n)
            | Equals ("keys", n) -> match Int32.TryParse n with (true, i) -> s <- s + sprintf "&cs=(%i.0, %i.0)" i i | _ -> ()
            | Equals ("m", m)
            | Equals ("c", m)
            | Equals ("creator", m)
            | Equals ("mapper", m) -> s <- s + "&mapper=" + m
            | _ -> ()
        ) filter
        s <- s + "&title=" + Uri.EscapeDataString title
        s <- s + "&offset=" + page.ToString()
        fun (searchContainer: SearchContainer) callback ->
            download_json_switch.Request(s,
                fun data ->
                match data with
                | Some d -> 
                    sync(fun () ->
                        for p in d.beatmaps do searchContainer.Items.Add(BeatmapImportCard p)
                        if d.result_count < 0 || d.result_count > d.beatmaps.Count then
                            SearchContainerLoader(search filter (page + 1) searchContainer)
                            |> searchContainer.Items.Add
                    )
                | None -> ()
                callback()
            )

    let tab =
        let searchContainer =
            SearchContainer(
                (search [] 0),
                (fun searchContainer filter -> searchContainer.Items.Clear(); searchContainer.Items.Add(new SearchContainerLoader(search filter 0 searchContainer))),
                Position = Position.TrimBottom(60.0f)
            )
        StaticContainer(NodeType.Switch(K searchContainer))
        |+ searchContainer
        |+ Text(L"imports.disclaimer.osu", Position = Position.SliceBottom(60.0f))