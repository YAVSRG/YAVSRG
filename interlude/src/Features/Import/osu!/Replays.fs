namespace Interlude.Features.Import.osu

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Data.OsuClientInterop
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Score

type ImportReplayPage(replay: OsuScoreDatabase_Score, chart: Chart, show_replay: Score -> unit) =
    inherit Page()

    let detected_rate =
        Data.Library.Imports.detect_rate_mod replay.FilePath.Value
        |> Option.defaultValue 1.0f<rate>

    // todo: even better detection using the fact that replay file contains md5 of original .osu
    // strategy 1: store original .osu md5s on all osu imports, search for it
    // strategy 2: if strategy 1 fails, open up the osu!.db if available, search for chart, convert and load it temporarily
    // if both strategies fail warn user that it's gonna try its best on the selected chart but that may be wrong

    let rate = Setting.bounded (0.5f<rate>, 3.0f<rate>) detected_rate |> Setting.roundf_uom 2

    let import() =
        match OsuReplay.to_score replay chart chart.FirstNote rate.Value with
        | Ok score -> show_replay score
        | Error reason -> Notifications.error ("Replay import failed", reason)

    override this.Content() =
        page_container()
        |+ PageSetting("Rate", Slider(Setting.uom rate)).Pos(0)
        |+ PageButton("Import!", import).Pos(3)
        :> Widget
    override this.Title = "Import replay"
    override this.OnClose() = ()

module Replay =

    let show_replay (chart_meta: ChartMeta) (chart: Chart) (score: Score) =
        SelectedChart.change(chart_meta, LibraryContext.None, true)
        SelectedChart.when_loaded true
        <| fun _ ->
            if Screen.change_new
                (fun () -> ScoreScreen(ScoreInfo.from_score chart_meta chart Rulesets.current score, (Gameplay.ImprovementFlags.None, None), false))
                Screen.Type.Score
                Transitions.EnterGameplayNoFadeAudio
            then Menu.Exit()

    let figure_out_replay (replay: OsuReplay) =

        // Strategy 1: Scan chart database for a chart that was imported from this .osu's md5
        let database_match =
            Content.Charts.Entries
            |> Seq.tryPick (fun chart_meta ->
                match
                    chart_meta.Origins
                    |> Seq.tryPick (
                        function
                        | ChartOrigin.Osu (md5, _, _, rate, first_note) when md5 = replay.BeatmapHash -> Some (rate, first_note)
                        | _ -> None
                    )
                with
                | None -> None
                | Some (rate, first_note) ->
                    match ChartDatabase.get_chart chart_meta.Hash Content.Charts with
                    | Error reason -> Logging.Error "Failed to load chart matching replay: %s" reason; None
                    | Ok chart -> Some (chart, chart_meta, rate, first_note / float32 rate)
            )
        match database_match with
        | Some (chart, chart_meta, rate, first_note) ->
            match OsuReplay.to_score replay chart first_note rate with
            | Ok score -> show_replay chart_meta chart score
            | Error reason -> Notifications.error ("Replay import failed", reason)
        | None ->

        // Strategy 2: Ask the user some details then assume it's the chart they have selected
        Logging.Info "No chart in the database matched MD5 hash '%s' for dropped replay" replay.BeatmapHash
        match SelectedChart.CACHE_DATA, SelectedChart.CHART with
        | Some cc, Some chart ->
            Menu.Exit()
            ImportReplayPage(
                replay,
                chart,
                show_replay cc chart
            )
                .Show()
        | _ -> ()