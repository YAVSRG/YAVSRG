namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Calculator
open Prelude.Mods
open Interlude.UI
open Interlude.Features.Gameplay

type GameplayInfo() =
    inherit Container(NodeType.None)

    let never_played = %"levelselect.last_played.never"

    let mutable rating = 0.0f
    let mutable notecounts = ""
    let mutable last_played = K never_played
    let mutable mod_string = "--"
    let mutable mod_status = ModStatus.Ranked

    let refresh(info: LoadedChartInfo) =
        rating <- info.Difficulty.Overall
        notecounts <- info.NotecountsString
        last_played <-
            let mutable ts = info.SaveData.LastPlayed
            let text (ts: int64) =
                if ts <= 0L then never_played else [ Timestamp.since info.SaveData.LastPlayed |> format_timespan ] %> "levelselect.last_played"
            let mutable t = text ts
            fun () ->
                if info.SaveData.LastPlayed <> ts then
                    ts <- info.SaveData.LastPlayed
                    t <- text ts
                t
        mod_string <- ModState.format (SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
        mod_status <- info.WithMods.Status

    static member HEIGHT = 230.0f

    override this.Init(parent) =
        SelectedChart.on_chart_change_finished.Add refresh
        SelectedChart.on_chart_update_finished.Add refresh
        SelectedChart.if_loaded refresh

        this.Add(
            GameplayInfoBoxes()
                .Position(Position.SliceT(100.0f).Shrink(10.0f))
        )

        base.Init parent

    override this.Draw() =

        let play_info = this.Bounds.SliceT(90.0f, 40.0f).ShrinkX(15.0f)
        Text.fill_b (Style.font, mod_string + (if SelectedChart.autoplay then ", AP" else ""), play_info, Colors.text, Alignment.LEFT)
        Text.fill_b (Style.font, last_played(), play_info, Colors.text, Alignment.RIGHT)
        match mod_status with
        | ModStatus.Unstored ->
            let x = Text.measure(Style.font, mod_string) * play_info.Height * 0.6f + 10.0f
            Text.fill_b (Style.font, Icons.X_CIRCLE + " " + %"mods.mod_status.unstored", play_info.ShrinkL x, Colors.text_greyout, Alignment.LEFT)
        | ModStatus.Unranked ->
            let x = Text.measure(Style.font, mod_string) * play_info.Height * 0.6f + 10.0f
            Text.fill_b (Style.font, Icons.ALERT_CIRCLE + " " + %"mods.mod_status.unranked", play_info.ShrinkL(x).ShrinkY(2.0f), Colors.text_yellow_2, Alignment.LEFT)
        | _ -> ()

        let chart_info = this.Bounds.SliceT(130.0f, 30.0f).ShrinkX(15.0f)
        Text.fill_b (Style.font, (match SelectedChart.CACHE_DATA with Some chart_meta -> chart_meta.DifficultyName | None -> ""), chart_info.SlicePercentL 0.5f, Colors.text_subheading, Alignment.LEFT)
        Text.fill_b (Style.font, notecounts, chart_info, Colors.text_subheading, Alignment.RIGHT)

        let three_icon_infos = this.Bounds.SliceT(155.0f, 70.0f).ShrinkX(15.0f)
        Text.fill_b (Style.font, sprintf "%s %.2f" Icons.STAR rating, three_icon_infos, (Colors.white, Difficulty.color rating), Alignment.LEFT)
        Text.fill_b (Style.font, SelectedChart.FMT_BPM, three_icon_infos, Colors.text, Alignment.CENTER)
        Text.fill_b (Style.font, SelectedChart.FMT_DURATION, three_icon_infos, Colors.text, Alignment.RIGHT)

        base.Draw()