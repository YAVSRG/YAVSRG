namespace Interlude.Features.Import.osu

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

type private BeatmapDownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

type private BeatmapImportCard(data: MinoBeatmapSet) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            )
        )
        
    static let STAT_WIDTH = 150.0f

    let mutable status = NotDownloaded
    let mutable progress = 0.0f

    let download () =
        if status = NotDownloaded || status = DownloadFailed then

            let task_tracking = TaskTracking.add data.title
            let progress_callback = fun p ->
                match p with
                | Data.Downloading percent -> progress <- percent
                | _ -> ()
                task_tracking.Progress <- p

            let task = OnlineImports.download_osu_set(sprintf "https://catboy.best/d/%in" data.id, Content.Charts, Content.UserData, progress_callback)
            import_queue.Request(task,
                function
                | Ok result ->
                    Notifications.task_feedback (
                        Icons.DOWNLOAD,
                        %"notification.install_song",
                        [data.title; result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.install_song.body"
                    )
                    Content.TriggerChartAdded()
                    progress <- 1.0f
                    status <- Installed
                | Error reason ->
                    Logging.Error "Error importing %s: %s" data.title reason
                    Notifications.error (%"notification.install_song_failed", data.title)
                    status <- DownloadFailed
            )

            status <- Downloading

    let fill, border, ranked_status =
        match data.status with
        | "ranked" -> Colors.cyan, Colors.cyan_accent, %"beatmap_browser.status.ranked"
        | "qualified" -> Colors.green, Colors.green_accent, %"beatmap_browser.status.qualified"
        | "loved" -> Colors.pink, Colors.pink_accent, %"beatmap_browser.status.loved"
        | "pending" -> Colors.grey_2, Colors.grey_1, %"beatmap_browser.status.pending"
        | "wip" -> Colors.grey_2, Colors.grey_1, %"beatmap_browser.status.wip"
        | "graveyard"
        | _ -> Colors.grey_2, Colors.grey_1, %"beatmap_browser.status.graveyard"

    let beatmaps = data.beatmaps |> Array.filter (fun x -> x.mode = "mania")

    let keymodes_string =
        let modes =
            beatmaps
            |> Seq.map (fun bm -> int bm.cs)
            |> Seq.filter (fun k -> k >= 3 && k <= 10)
            |> Seq.distinct
            |> Seq.sort
            |> Array.ofSeq

        if modes.Length > 3 then
            sprintf "%i-%iK" modes.[0] modes.[modes.Length - 1]
        else
            modes |> Seq.map (fun k -> sprintf "%iK" k) |> String.concat ", "
        
    static member HEIGHT = 80.0f

    override this.Init(parent) =
        this
        |+ Frame(
            Fill = (fun () -> if this.Focused then fill.O3 else fill.O2),
            Border = fun () -> if this.Focused then Colors.white else border.O2
        )
        //|+ Button(Icons.OPEN_IN_BROWSER,
        //    fun () -> openUrl(sprintf "https://osu.ppy.sh/beatmapsets/%i" data.beatmapset_id)
        //    ,
        //    Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f))
        |* MouseListener().Button(this)
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        match status with
        | Downloading -> Render.rect (this.Bounds.SliceL(this.Bounds.Width * progress)) Colors.white.O1
        | _ -> ()

        Text.fill_b (
            Style.font,
            data.title,
            this.Bounds.SliceT(45.0f).Shrink(Style.PADDING * 2.0f, 0.0f),
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            data.artist + "  •  " + data.creator,
            this.Bounds.SliceB(45.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING),
            Colors.text_subheading,
            Alignment.LEFT
        )

        let download_bounds = this.Bounds.SliceR(BeatmapImportCard.HEIGHT).Shrink(Style.PADDING * 2.0f)
        Render.rect download_bounds Colors.shadow_2.O2
        Text.fill_b (
            Style.font,
            (match status with
             | NotDownloaded -> Icons.DOWNLOAD
             | Downloading -> Icons.REFRESH_CW
             | DownloadFailed -> Icons.X
             | Installed -> Icons.CHECK),
            download_bounds.Shrink(Style.PADDING * 2.0f),
            (match status with
             | NotDownloaded -> if this.Focused then Colors.text_yellow_2 else Colors.text
             | Downloading -> Colors.text_yellow_2
             | DownloadFailed -> Colors.text_red
             | Installed -> Colors.text_green),
            Alignment.CENTER
        )

        let status = this.Bounds.ShrinkR(BeatmapImportCard.HEIGHT).SlicePercentB(0.5f).SliceR(STAT_WIDTH).ShrinkT(Style.PADDING)
        Render.rect status Colors.shadow_2.O2
        Text.fill_b (
            Style.font,
            ranked_status,
            status.Shrink(Style.PADDING, 0.0f).ShrinkB(Style.PADDING),
            (border, Colors.shadow_2),
            Alignment.CENTER
        )
        
        let favourites = this.Bounds.ShrinkR(BeatmapImportCard.HEIGHT).SlicePercentT(0.5f).SliceR(STAT_WIDTH).ShrinkB(Style.PADDING)
        Render.rect favourites Colors.shadow_2.O2
        Text.fill_b (
            Style.font,
            (sprintf "%s %i" Icons.HEART data.favourite_count),
            favourites.Shrink(Style.PADDING, 0.0f).ShrinkB(Style.PADDING),
            Colors.text_subheading,
            Alignment.CENTER
        )
        
        let keymodes = this.Bounds.ShrinkR(BeatmapImportCard.HEIGHT).SlicePercentB(0.5f).ShrinkR(STAT_WIDTH + Style.PADDING).SliceR(STAT_WIDTH).ShrinkT(Style.PADDING)
        Render.rect keymodes Colors.shadow_2.O2
        Text.fill_b (
            Style.font,
            keymodes_string,
            keymodes.Shrink(Style.PADDING, 0.0f).ShrinkB(Style.PADDING),
            Colors.text_subheading,
            Alignment.CENTER
        )
        
        let playcount = this.Bounds.ShrinkR(BeatmapImportCard.HEIGHT).SlicePercentT(0.5f).ShrinkR(STAT_WIDTH + Style.PADDING).SliceR(STAT_WIDTH).ShrinkB(Style.PADDING)
        Render.rect playcount Colors.shadow_2.O2
        Text.fill_b (
            Style.font,
            (sprintf "%s %i" Icons.PLAY data.play_count),
            playcount.Shrink(Style.PADDING, 0.0f).ShrinkB(Style.PADDING),
            Colors.text_subheading,
            Alignment.CENTER
        )

        if this.Focused && Mouse.x () > this.Bounds.Right - 600.0f then
            let popover_bounds =
                Rect.FromSize(
                    this.Bounds.Right - 600.0f,
                    this.Bounds.Bottom + Style.PADDING * 2.0f,
                    600.0f,
                    45.0f * float32 beatmaps.Length
                )

            Render.rect popover_bounds Colors.shadow_2.O3
            let mutable y = 0.0f

            for beatmap in beatmaps do
                Text.fill_b (
                    Style.font,
                    beatmap.version,
                    popover_bounds.SliceT(45.0f).Translate(0.0f, y).Shrink(10.0f, 5.0f),
                    Colors.text,
                    Alignment.LEFT
                )

                Text.fill_b (
                    Style.font,
                    sprintf "%.2f*" beatmap.difficulty_rating,
                    popover_bounds.SliceT(45.0f).Translate(0.0f, y).Shrink(10.0f, 5.0f),
                    Colors.text,
                    Alignment.RIGHT
                )

                y <- y + 45.0f

    member private this.Download() = download ()

type SortingDropdown =

    static member Create(options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =

        let mutable display_value =
            Seq.find (fun (id, _) -> id = setting.Value) options |> snd

        let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))

        let toggle_dropdown() =
            dropdown_wrapper.Toggle(fun () ->
                Dropdown
                    {
                        Items = options
                        ColorFunc = K Colors.text
                        Setting =
                            setting
                            |> Setting.trigger (fun v ->
                                display_value <- Seq.find (fun (id, _) -> id = v) options |> snd
                            )
                    }
            )

        let LABEL_AREA_SIZE = 120.0f

        NavigationContainer.Row()
            .WrapNavigation(false)
            .With(
                AngledButton(
                    label + ":",
                    toggle_dropdown,
                    Colors.cyan.O3
                )
                    .Hotkey(bind)
                    .Position(Position.SliceL LABEL_AREA_SIZE),

                AngledButton(
                    (fun () ->
                        sprintf
                            "%s %s"
                            display_value
                            (if reverse.Value then
                                    Icons.CHEVRONS_DOWN
                                else
                                    Icons.CHEVRONS_UP)
                    ),
                    (fun () -> reverse.Value <- not reverse.Value),
                    Colors.cyan_shadow.O3
                )
                    .LeanRight(false)
                    .Position(Position.ShrinkL(LABEL_AREA_SIZE + AngledButton.LEAN_AMOUNT)),

                dropdown_wrapper
            )