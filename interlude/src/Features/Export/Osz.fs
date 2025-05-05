namespace Interlude.Features.Export

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Mods
open Prelude.Data.Library
open Interlude.UI

module OsuExport =

    let export_chart_without_mods (chart: Chart) (chart_meta: ChartMeta) (options: OsuExportOptions) : unit =
        match Exports.create_osz options chart chart_meta (get_game_folder "Exports") with
        | Ok _ ->
            open_directory (get_game_folder "Exports")
            Notifications.action_feedback(Icons.CHECK, %"notification.song_exported.title", "")
        | Error err ->
            Notifications.error(%"notification.song_export_failed.title", %"notification.song_export_failed.body")
            Logging.Error "Error exporting '%s' as osz: %O" chart_meta.Title err

    let export_chart_with_mods (chart: ModdedChart) (chart_meta: ChartMeta) (options: OsuExportOptions) : unit =
        let mod_string =
            chart.ModsApplied
            |> ModState.in_priority_order
            |> Seq.map (fun (id, _, state) -> Mods.name id (Some state))
            |> String.concat ", "

        let chart_with_mods : Chart =
            { Keys = chart.Keys; Notes = chart.Notes; SV = chart.SV; BPM = chart.BPM }
        let meta_with_mods =
            { chart_meta with DifficultyName = chart_meta.DifficultyName.Trim() + sprintf " (+%s)" mod_string }

        match Exports.create_osz options chart_with_mods meta_with_mods (get_game_folder "Exports") with
        | Ok _ ->
            open_directory (get_game_folder "Exports")
            Notifications.action_feedback(Icons.CHECK, %"notification.song_exported.title", "")
        | Error err ->
            Notifications.error(%"notification.song_export_failed.title", %"notification.song_export_failed.body")
            Logging.Error "Error exporting '%s' as osz: %O" chart_meta.Title err

    let bulk_export (charts: (Result<Chart, string> * ChartMeta) seq) (options: OsuExportOptions) : unit =
        try
            let export_path = System.IO.Path.Combine(get_game_folder "Exports", sprintf "export-%i" (Timestamp.now()))
            System.IO.Directory.CreateDirectory export_path |> ignore

            open_directory export_path

            let mutable ok = 0
            let mutable failed = 0

            for maybe_chart, chart_meta in charts do
                match maybe_chart with
                | Ok chart ->
                    match Exports.create_osz options chart chart_meta export_path with
                    | Ok _ ->
                        ok <- ok + 1
                    | Error err ->
                        failed <- failed + 1
                        Logging.Error "Error exporting '%s' as osz: %O" chart_meta.Title err
                | Error reason ->
                    failed <- failed + 1
                    Logging.Error "Error fetching chart for '%s' to export: %s" chart_meta.Hash reason

            Notifications.action_feedback(Icons.CHECK, %"notification.bulk_exported.title", [ok.ToString(); failed.ToString()] %> "notification.bulk_exported.body")
        with err ->
            Logging.Error "Unexpected error bulk exporting charts: %O" err

type OsuExportOptionsPage(title: string, selected_mods: ModState, on_submit: bool -> OsuExportOptions -> unit) =
    inherit Page()

    let od = Setting.bounded (0.0f, 10.0f) 8.0f
    let hp = Setting.bounded (0.0f, 10.0f) 8.0f
    let apply_mods = Setting.simple (not selected_mods.IsEmpty)

    override this.Content() =
        page_container()
            .With(
                PageSetting(%"osz_export.od", Slider(od, Step = 0.1f))
                    .Pos(0),
                PageSetting(%"osz_export.hp", Slider(hp, Step = 0.1f))
                    .Pos(2),
                PageSetting(%"osz_export.apply_mods", Checkbox apply_mods)
                    .Pos(5)
                    .Conditional(K (not selected_mods.IsEmpty)),
                PageButton
                    .Once(
                        %"osz_export.confirm",
                        fun () ->
                            on_submit apply_mods.Value { OD = float od.Value; HP = float hp.Value }
                            Menu.Exit()
                    )
                    .Pos(8)
            )

    override this.Title = title