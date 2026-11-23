namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

type ScoreGraphSettingsPage(keys: int, apply_column_filter: unit -> unit) =
    inherit Page()

    let mutable column_filter_changed = false

    let column_filter_setting k =
        Setting.make
            (fun v ->
                column_filter_changed <- true
                ScoreGraphSettings.column_filter.[k] <- v
            )
            (fun () -> ScoreGraphSettings.column_filter.[k])

    let column_filter_ui, _ =
        refreshable_row
            (fun () -> keys)
            (fun k _ ->
                Checkbox(column_filter_setting k).Position(Position.SliceL(50.0f).Translate(float32 k * 80.0f, 0.0f))
            )

    override this.Content() : Widget =
        this.OnClose(fun () ->
            if column_filter_changed then
                apply_column_filter()
        )

        page_container()
            .With(
                PageSetting(
                    %"score.graph.settings.line_mode",
                    SelectDropdown(
                        [|
                            ScoreGraphLineMode.None, %"score.graph.settings.line_mode.none"
                            ScoreGraphLineMode.Combo, %"score.graph.settings.line_mode.combo"
                            ScoreGraphLineMode.Mean, %"score.graph.settings.line_mode.mean"
                            ScoreGraphLineMode.StandardDeviation, %"score.graph.settings.line_mode.standard_deviation"
                            ScoreGraphLineMode.Accuracy, %"score.graph.settings.line_mode.accuracy"
                            ScoreGraphLineMode.MA, %"score.graph.settings.line_mode.ma"
                            ScoreGraphLineMode.PA, %"score.graph.settings.line_mode.pa"
                        |],
                        options.ScoreGraphLineMode
                    )
                )
                    .Pos(0),
                PageSetting(
                    %"score.graph.settings.line_color",
                    SelectDropdown(
                        [|
                            ScoreGraphLineColor.White, %"score.graph.settings.line_color.white"
                            ScoreGraphLineColor.Lamp, %"score.graph.settings.line_color.lamp"
                            ScoreGraphLineColor.Grade, %"score.graph.settings.line_color.grade"
                        |],
                        options.ScoreGraphLineColor
                    )
                )
                    .Conditional(fun () -> options.ScoreGraphLineMode.Value <> ScoreGraphLineMode.None)
                    .Pos(2),
                PageSetting(%"score.graph.settings.line_on_top", Checkbox(options.ScoreGraphLineOnTop))
                    .Conditional(fun () -> options.ScoreGraphLineMode.Value <> ScoreGraphLineMode.None)
                    .Pos(4),
                PageSetting(%"score.graph.settings.only_releases", Checkbox ScoreGraphSettings.only_releases).Pos(7),
                PageSetting(%"score.graph.settings.column_filter", column_filter_ui).Pos(9, 2, PageWidth.Full),
                PageSetting(
                    %"score.graph.settings.scale",
                    Slider(ScoreGraphSettings.scale, Step = 0.25f, Format = (fun x -> sprintf "%.0f%%" (x * 100.0f)))
                )
                    .Pos(11),
                PageSetting(%"score.graph.settings.windows_background", Checkbox(options.ScoreGraphWindowBackground))
                    .Pos(13),
                PageSetting(
                    %"score.graph.settings.hover_info",
                    SelectDropdown(
                        [|
                            false, %"score.graph.settings.hover_info.cumulative"
                            true, %"score.graph.settings.hover_info.slice"
                        |],
                        ScoreGraphSettings.show_slice
                    )
                )
                    .Help(
                        Help
                            .Info("score.graph.settings.hover_info")
                            .Hotkey(%"score.graph.settings.hover_info_hint", "graph_alt_info")
                    )
                    .Pos(16),
                PageSetting(%"score.graph.settings.slice_size", Slider.Percent(ScoreGraphSettings.slice_size))
                    .Help(Help.Info("score.graph.settings.slice_size"))
                    .Pos(18)
            )

    override this.Title = %"score.graph.settings"
