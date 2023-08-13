namespace Interlude.Features.Import

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Utils

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
    inherit StaticContainer(NodeType.Button(fun () -> Style.click.Play(); this.Download()))
            
    let mutable status = NotDownloaded
    let mutable progress = 0.0f
    let download() =
        if status = NotDownloaded || status = DownloadFailed then
            let target = Path.Combine(getDataPath "Downloads", Guid.NewGuid().ToString() + ".osz")
            WebServices.download_file.Request((sprintf "https://api.chimu.moe/v1/download/%i?n=1" data.beatmapset_id, target, fun p -> progress <- p),
                fun completed ->
                    if completed then Library.Imports.auto_convert.Request((target, true),
                        fun b ->
                            if b then charts_updated_ev.Trigger()
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
        |+ Frame(NodeType.None, Fill = (fun () -> if this.Focused then fill.O3 else fill.O2), Border = fun () -> if this.Focused then Colors.white else border.O2)
        |* Clickable.Focus this
        //|+ Button(Icons.open_in_browser,
        //    fun () -> openUrl(sprintf "https://osu.ppy.sh/beatmapsets/%i" data.beatmapset_id)
        //    ,
        //    Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f))

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    override this.Draw() =
        base.Draw()

        match status with
        | Downloading -> Draw.rect (this.Bounds.SliceLeft(this.Bounds.Width * progress)) Colors.white.O1
        | _ -> ()

        Text.drawFillB(Style.font, data.title, this.Bounds.SliceTop(50.0f).Shrink(10.0f, 0.0f), Colors.text, Alignment.LEFT)
        Text.drawFillB(Style.font, data.artist + "  •  " + data.mapper, this.Bounds.SliceBottom(45.0f).Shrink(10.0f, 5.0f), Colors.text_subheading, Alignment.LEFT)

        let status_bounds = this.Bounds.SliceBottom(40.0f).SliceRight(150.0f).Shrink(5.0f, 0.0f)
        Draw.rect status_bounds Colors.shadow_2.O2
        Text.drawFillB(Style.font, ranked_status, status_bounds.Shrink(5.0f, 0.0f).TrimBottom(5.0f), (border, Colors.shadow_2), Alignment.CENTER)

        let stat x text =
            let stat_bounds = this.Bounds.SliceBottom(40.0f).TrimRight(x).SliceRight(145.0f)
            Draw.rect stat_bounds Colors.shadow_2.O2
            Text.drawFillB(Style.font, text, stat_bounds.Shrink(5.0f, 0.0f).TrimBottom(5.0f), Colors.text_subheading, Alignment.CENTER)

        stat 150.0f (sprintf "%s %i" Icons.heart data.favorites)
        stat 300.0f (sprintf "%s %i" Icons.play data.play_count)
        stat 450.0f (sprintf "%iK" (int data.difficulty_cs))

        let download_bounds = this.Bounds.SliceTop(40.0f).SliceRight(300.0f).Shrink(5.0f, 0.0f)
        Draw.rect download_bounds Colors.shadow_2.O2
        Text.drawFillB(
            Style.font, 
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
                | NotDownloaded -> if this.Focused then Colors.text_yellow_2 else Colors.text
                | Downloading -> Colors.text_yellow_2
                | DownloadFailed -> Colors.text_red
                | Installed -> Colors.text_green
            ),
            Alignment.CENTER)

    member private this.Download() = download()

type private SortingDropdown(options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit StaticContainer(NodeType.None)
    
    let mutable displayValue = Seq.find (fun (id, _) -> id = setting.Value) options |> snd
    
    override this.Init(parent: Widget) =
        this 
        |+ StylishButton(
            ( fun () -> this.ToggleDropdown() ),
            K (label + ":"),
            !%Palette.HIGHLIGHT_100,
            Hotkey = bind,
            Position = Position.SliceLeft 120.0f)
        |* StylishButton(
            ( fun () -> reverse.Value <- not reverse.Value ),
            ( fun () -> sprintf "%s %s" displayValue (if reverse.Value then Icons.order_descending else Icons.order_ascending) ),
            !%Palette.DARK_100,
            TiltRight = false,
            Position = Position.TrimLeft 145.0f )
        base.Init parent
    
    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let d = Dropdown.Selector options snd (fun g -> displayValue <- snd g; setting.Set (fst g)) (fun () -> this.Dropdown <- None)
            d.Position <- Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.PADDING, 0.0f)
            d.Init this
            this.Dropdown <- Some d
    
    member val Dropdown : Dropdown option = None with get, set
    
    override this.Draw() =
        base.Draw()
        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        match this.Dropdown with
        | Some d -> d.Update(elapsedTime, moved)
        | None -> ()

module Beatmaps =

    let download_json_switch = 
        { new Async.SwitchService<string, BeatmapSearch option>()
            with override this.Handle(url) = WebServices.download_json_async<BeatmapSearch>(url)
        }

    type BeatmapSearch() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))

        let items = FlowContainer.Vertical<BeatmapImportCard>(80.0f, Spacing = 15.0f)
        let scroll = ScrollContainer.Flow(items, Margin = Style.PADDING, Position = Position.TrimTop(125.0f).TrimBottom(65.0f))
        let mutable filter : Filter = []
        let query_order = Setting.simple "date"
        let descending_order = Setting.simple true
        let mutable statuses = Set.singleton "Ranked"
        let mutable when_at_bottom : (unit -> unit) option = None

        let rec search (filter: Filter) (page: int) =
            when_at_bottom <- None
            let mutable s = "https://osusearch.com/api/search?modes=Mania&key=&query_order=" + (if descending_order.Value then "" else "-") + query_order.Value + "&statuses=" + String.concat "," statuses
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

            download_json_switch.Request(s,
                fun data ->
                match data with
                | Some d -> 
                    sync(fun () ->
                        for p in d.beatmaps do items.Add(BeatmapImportCard p)
                        if d.result_count < 0 || d.result_count > d.beatmaps.Count then
                            when_at_bottom <- Some (fun () -> search filter (page + 1))
                    )
                | None -> ()
            )

        let begin_search (filter: Filter) =
            search filter 0
            items.Clear()

        let status_button (status: string) (position: Position) (color: Color) =
            StylishButton(
                (fun () -> 
                    if statuses.Contains status then 
                        statuses <- Set.remove status statuses 
                    else statuses <- Set.add status statuses
                    begin_search filter
                ),
                (fun () -> if statuses.Contains status then Icons.check + " " + status else Icons.x + " " + status),
                (fun () -> if statuses.Contains status then color.O3 else color.O1),
                Position = position
            )

        override this.Focusable = items.Focusable
    
        override this.Init(parent) =
            begin_search filter
            this
            |+ (SearchBox(Setting.simple "", (fun (f: Filter) -> filter <- f; sync(fun () -> begin_search filter)), Position = Position.SliceTop 60.0f ))
            |+ Text(L"imports.disclaimer.osu", Position = Position.SliceBottom 55.0f)
            |+ scroll
            |+ (
                let r = status_button "Ranked" { Position.TrimTop(65.0f).SliceTop(50.0f) with Right = 0.18f %- 25.0f } Colors.cyan
                r.TiltLeft <- false
                r
               )
            |+ status_button 
                "Qualified"
                { Position.TrimTop(65.0f).SliceTop(50.0f) with Left = 0.18f %+ 0.0f; Right = 0.36f %- 25.0f }
                Colors.green
            |+ status_button 
                "Loved"
                { Position.TrimTop(65.0f).SliceTop(50.0f) with Left = 0.36f %+ 0.0f; Right = 0.54f %- 25.0f }
                Colors.pink
            |+ status_button 
                "Unranked"
                { Position.TrimTop(65.0f).SliceTop(50.0f) with Left = 0.54f %+ 0.0f; Right = 0.72f %- 25.0f }
                Colors.grey_2
            |* SortingDropdown(
                [
                    "play_count", "Play count"
                    "date", "Date"
                    "difficulty", "Difficulty"
                    "favorites", "Favourites"
                ],
                "Sort",
                query_order |> Setting.trigger (fun _ -> begin_search filter),
                descending_order |> Setting.trigger (fun _ -> begin_search filter),
                "sort_mode",
                Position = { Left = 0.72f %+ 0.0f; Top = 0.0f %+ 65.0f; Right = 1.0f %- 0.0f; Bottom = 0.0f %+ 115.0f })
            base.Init parent

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if when_at_bottom.IsSome && scroll.PositionPercent > 0.9f then
                when_at_bottom.Value()
                when_at_bottom <- None
    
        member private this.Items = items

    let tab = BeatmapSearch()