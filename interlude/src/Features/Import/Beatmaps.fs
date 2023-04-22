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
        favorites: int
        play_count: int
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
            WebServices.download_file.Request((sprintf "https://api.nerinyan.moe/d/%i?noVideo=1&noHitsound=1&noStoryboard=1" data.beatmapset_id, target, fun p -> progress <- p),
                fun completed ->
                    if completed then Library.Imports.auto_convert.Request(target,
                        fun b ->
                            if b then LevelSelect.refresh_all()
                            Notifications.task_feedback (Icons.download, L"notification.install_song", data.title)
                            File.Delete target
                            status <- if b then Installed else DownloadFailed
                    )
                    else status <- DownloadFailed
                )
            status <- Downloading

    let fill, border, ranked_status =
        match data.beatmap_status with
        | BeatmapStatus.RANKED -> Colors.cyan, Colors.cyan_accent, "Ranked"
        | BeatmapStatus.QUALIFIED -> Colors.green, Colors.green_accent, "Qualified"
        | BeatmapStatus.LOVED -> Colors.pink, Colors.pink_accent, "Loved"
        | BeatmapStatus.PENDING -> Colors.grey_2, Colors.grey_1, "Pending"
        | BeatmapStatus.WORK_IN_PROGRESS -> Colors.grey_2, Colors.grey_1, "WIP"
        | BeatmapStatus.GRAVEYARD
        | _ -> Colors.grey_2, Colors.grey_1, "Graveyard"

    do

        this
        |+ Frame(NodeType.None, Fill = (fun () -> if this.Focused then fill.O3 else Colors.shadow_2.O2), Border = fun () -> if this.Focused then Colors.white else border.O2)
        |* Clickable.Focus this
        //|+ Text(data.artist + " - " + data.title,
        //    Align = Alignment.LEFT,
        //    Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 400.0f; Bottom = 1.0f %- 30.0f })
        //|+ Text("Created by " + data.mapper,
        //    Align = Alignment.LEFT,
        //    Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 40.0f; Right = Position.max; Bottom = Position.max })
        //|+ Text(sprintf "%.2f*   %iBPM   %iK" data.difficulty data.bpm (int data.difficulty_cs),
        //    Align = Alignment.RIGHT,
        //    Position = Position.TrimRight(160.0f).Margin(5.0f, 20.0f))
        //|+ Button(Icons.open_in_browser,
        //    fun () -> openUrl(sprintf "https://osu.ppy.sh/beatmapsets/%i" data.beatmapset_id)
        //    ,
        //    Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f))
        //|* Button(Icons.download, download,
        //    Position = Position.SliceRight(80.0f).Margin(5.0f, 10.0f))

    override this.Draw() =
        base.Draw()

        match status with
        | Downloading -> Draw.rect (this.Bounds.SliceLeft(this.Bounds.Width * progress)) Colors.white.O1
        | _ -> ()

        Text.drawFillB(Style.baseFont, data.title, this.Bounds.SliceTop(50.0f).Shrink(5.0f, 0.0f), Colors.text, Alignment.LEFT)
        Text.drawFillB(Style.baseFont, data.artist + "  •  " + data.mapper, this.Bounds.TrimTop(45.0f).SliceTop(35.0f).Shrink(5.0f, 0.0f), Colors.text_subheading, Alignment.LEFT)

        let status_bounds = this.Bounds.SliceBottom(45.0f).SliceLeft(150.0f).Shrink(5.0f, 0.0f)
        Draw.rect status_bounds Colors.shadow_2.O2
        Text.drawFillB(Style.baseFont, ranked_status, status_bounds.Shrink(5.0f, 0.0f).TrimBottom(5.0f), (border, Colors.shadow_2), Alignment.CENTER)

        let stat x text =
            let stat_bounds = this.Bounds.SliceBottom(45.0f).TrimLeft(x).SliceLeft(145.0f)
            Draw.rect stat_bounds Colors.shadow_2.O2
            Text.drawFillB(Style.baseFont, text, stat_bounds.Shrink(5.0f, 0.0f).TrimBottom(5.0f), Colors.text, Alignment.CENTER)

        stat 150.0f (sprintf "%s %i" Icons.heart data.favorites)
        stat 300.0f (sprintf "%s %i" Icons.play data.play_count)
        stat 450.0f (sprintf "%iK" (int data.difficulty_cs))

        let download_bounds = this.Bounds.SliceBottom(45.0f).SliceRight(300.0f).Shrink(5.0f, 0.0f)
        Draw.rect download_bounds Colors.shadow_2.O2
        Text.drawFillB(
            Style.baseFont, 
            (
                match status with
                | NotDownloaded -> Icons.download + " Download"
                | Downloading -> Icons.download + " Downloading .."
                | DownloadFailed -> Icons.x + " Error"
                | Installed -> Icons.check + " Downloaded"
            ),
            download_bounds.Shrink(5.0f, 0.0f).TrimBottom(5.0f),
            (
                match status with
                | NotDownloaded -> Colors.text
                | Downloading -> Colors.text_yellow_2
                | DownloadFailed -> Colors.text_red
                | Installed -> Colors.text_green
            ),
            Alignment.CENTER)

    member private this.Download() = download()

module Beatmaps =

    let download_json_switch = 
        { new Async.SwitchService<string, BeatmapSearch option>()
            with override this.Handle(url) = WebServices.download_json_async<BeatmapSearch>(url)
        }
    
    let rec search (filter: Filter) (page: int) : PopulateFunc =
        let mutable s = "https://osusearch.com/api/search?modes=Mania&query_order=play_count&key="
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
                130.0f,
                Position = Position.TrimBottom(60.0f)
            )
        StaticContainer(NodeType.Switch(K searchContainer))
        |+ searchContainer
        |+ Text(L"imports.disclaimer.osu", Position = Position.SliceBottom(60.0f))