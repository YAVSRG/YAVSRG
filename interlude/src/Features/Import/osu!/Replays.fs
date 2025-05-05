namespace Interlude.Features.Import.osu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Prelude.Data.OsuClientInterop
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Score

type ImportReplayPage(replay: OsuScoreDatabase_Score, chart: Chart, show_replay: Score -> unit) =
    inherit Page()

    let info =
        CalloutCard(
            Callout.Normal
                .Icon(Icons.ALERT_OCTAGON)
                .Title("Attention")
                .Body("The beatmap for this replay could not be found.")
                .Body("This screen lets you manually try to import a replay IF\n - You are sure you currently have the right map selected\n - You are sure you have the right rate selected")
                .Body("If you haven't reimported osu! charts since 0.7.27.6, you should and this warning will probably stop showing up."),
            Colors.red_accent,
            Colors.red.O3)

    let detected_rate =
        detect_rate_mod replay.FilePath.Value
        |> Option.defaultValue 1.0f<rate>

    let rate = Setting.bounded (0.5f<rate>, 3.0f<rate>) detected_rate |> Setting.roundf_uom 2

    let import () : unit =
        match OsuReplay.to_score replay chart chart.FirstNote rate.Value with
        | Ok score -> show_replay score
        | Error reason -> Notifications.error ("Replay import failed", reason)

    override this.Content() =
        page_container()
        |+ PageSetting("Rate", Slider(Setting.uom rate)).Pos(0)
        |+ PageButton("Import!", import).Pos(3)
        |+ info.Pos(6, 9, PageWidth.Custom (info :> IWidth).Width)
        :> Widget
    override this.Title = "Import replay"

module Replay =

    let show_replay (played_by: string) (chart_meta: ChartMeta) (chart: Chart) (score: Score) : unit =
        let score_info =
            { ScoreInfo.from_score chart_meta chart Rulesets.current score with
                PlayedBy = ScorePlayedBy.Username played_by
            }
        SelectedChart.change(chart_meta, LibraryContext.None, true)
        SelectedChart.when_loaded true
        <| fun _ ->
            if Screen.change_new
                (fun () -> ScoreScreen(score_info, (ImprovementFlags.None, None), false))
                ScreenType.Score
                Transitions.EnterGameplayNoFadeAudio
            then Menu.Exit()

    let figure_out_replay (replay: OsuReplay) : unit =

        // Strategy 1: Scan chart database for a chart that was imported from this .osu's md5
        let database_match =
            Content.Charts.Entries
            |> Seq.tryPick (fun chart_meta ->
                match
                    chart_meta.Origins
                    |> Seq.tryPick (
                        function
                        | ChartOrigin.Osu osu when osu.Md5 = replay.BeatmapHash -> Some (osu.SourceRate, osu.FirstNoteOffset)
                        | _ -> None
                    )
                with
                | None -> None
                | Some (rate, first_note) ->
                    match ChartDatabase.get_chart chart_meta.Hash Content.Charts with
                    | Error reason -> Logging.Error "Failed to load chart matching replay: %s" reason; None
                    | Ok chart -> Some (chart, chart_meta, rate, first_note)
            )
        match database_match with
        | Some (chart, chart_meta, rate, first_note) ->
            match OsuReplay.to_score replay chart first_note rate with
            | Ok score -> show_replay replay.Player chart_meta chart score
            | Error reason -> Notifications.error ("Replay import failed", reason)
        | None ->

        // Strategy 2: Ask the user some details then assume it's the chart they have selected
        Logging.Info "No chart in the database matched MD5 hash '%s' for dropped replay" replay.BeatmapHash
        match SelectedChart.CACHE_DATA, SelectedChart.CHART with
        | Some chart_meta, Some chart ->
            Menu.Exit()
            ImportReplayPage(
                replay,
                chart,
                show_replay replay.Player chart_meta chart
            )
                .Show()
        | _ -> ()