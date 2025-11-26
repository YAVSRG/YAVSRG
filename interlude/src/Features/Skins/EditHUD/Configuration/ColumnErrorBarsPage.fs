namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type ColumnErrorBarsPage() =
    inherit Page()

    let config = Content.HUD

    let show_guide = Setting.simple config.ColumnErrorBarsShowGuide
    let guide_thickness = Setting.percentf config.ColumnErrorBarsGuideThickness
    let show_non_judgements = Setting.simple config.ColumnErrorBarsShowNonJudgements

    let thickness = config.ColumnErrorBarsThickness |> Setting.bounded (1.0f, 25.0f)
    let width = config.ColumnErrorBarsWidth |> Setting.bounded (5.0f, 100.0f)

    let windows_opacity = config.ColumnErrorBarsWindowsOpacity |> Setting.bounded (0.0f, 0.6f)

    let release_y_scale = config.ColumnErrorBarsReleasesYScale |> Setting.bounded (0.0f, 1.0f)
    let release_x_scale = config.ColumnErrorBarsReleasesXScale |> Setting.bounded (0.0f, 2.0f)

    let animation_time =
        config.ColumnErrorBarsFadeTime
        |> Setting.bounded (100.0f<ms / rate>, 2000.0f<ms / rate>)

    let enable_moving_average = Setting.simple config.ColumnErrorBarsMovingAverage
    let moving_average_sensitivity = config.ColumnErrorBarsMovingAverageSensitivity |> Setting.bounded (0.01f, 1.0f)
    let moving_average_color = Setting.simple config.ColumnErrorBarsMovingAverageColor

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                ColumnErrorBarsWidth = width.Value
                ColumnErrorBarsThickness = thickness.Value
                ColumnErrorBarsShowGuide = show_guide.Value
                ColumnErrorBarsShowNonJudgements = show_non_judgements.Value
                ColumnErrorBarsGuideThickness = guide_thickness.Value
                ColumnErrorBarsReleasesXScale = release_x_scale.Value
                ColumnErrorBarsReleasesYScale = release_y_scale.Value
                ColumnErrorBarsWindowsOpacity = windows_opacity.Value
                ColumnErrorBarsFadeTime = animation_time.Value
                ColumnErrorBarsMovingAverage = enable_moving_average.Value
                ColumnErrorBarsMovingAverageSensitivity = moving_average_sensitivity.Value
                ColumnErrorBarsMovingAverageColor = moving_average_color.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.column_error_bars.width", Slider(width, Step = 1f))
                    .Help(Help.Info("hud.column_error_bars.width"))
                    .Pos(0),
                PageSetting(%"hud.column_error_bars.thickness", Slider(thickness, Step = 1f))
                    .Help(Help.Info("hud.column_error_bars.thickness"))
                    .Pos(2),
                PageSetting(%"hud.column_error_bars.show_non_judgements", Checkbox(show_non_judgements))
                    .Help(Help.Info("hud.column_error_bars.show_non_judgements"))
                    .Pos(4),
                PageSetting(%"hud.column_error_bars.release_y_scale", Slider.Percent(release_y_scale))
                    .Help(Help.Info("hud.column_error_bars.release_y_scale"))
                    .Pos(6),
                PageSetting(%"hud.column_error_bars.show_guide", Checkbox(show_guide))
                    .Help(Help.Info("hud.column_error_bars.show_guide"))
                    .Pos(8),
                PageSetting(%"hud.column_error_bars.guide_thickness", Slider.Percent(guide_thickness))
                    .Help(Help.Info("hud.column_error_bars.guide_thickness"))
                    .Pos(10)
                    .Conditional(show_guide.Get),
                PageSetting(%"hud.column_error_bars.release_x_scale", Slider.Percent(release_x_scale))
                    .Help(Help.Info("hud.column_error_bars.release_x_scale"))
                    .Pos(12),
                PageSetting(%"hud.column_error_bars.animationtime", Slider(Setting.uom animation_time, Step = 5f))
                    .Help(Help.Info("hud.column_error_bars.animationtime"))
                    .Pos(14),
                PageSetting(%"hud.column_error_bars.timing_windows_opacity", Slider.Percent(windows_opacity))
                    .Help(Help.Info("hud.column_error_bars.timing_windows_opacity"))
                    .Pos(16),
                PageSetting(%"hud.column_error_bars.moving_average", Checkbox(enable_moving_average))
                    .Help(Help.Info("hud.error_bar.moving_average_type"))
                    .Pos(18)
            )
            .WithConditional(
                (fun () -> enable_moving_average.Value),

                PageSetting(%"hud.column_error_bars.moving_average_sensitivity", Slider.Percent(moving_average_sensitivity, Step = 0.01f))
                    .Help(Help.Info("hud.column_error_bars.moving_average_sensitivity"))
                    .Pos(20),
                PageSetting(%"hud.column_error_bars.moving_average_color", ColorPicker(%"hud.column_error_bars.moving_average_color", moving_average_color, true))
                    .Help(Help.Info("hud.column_error_bars.moving_average_color"))
                    .Pos(22)
            )

    override this.Title = %"hud.column_error_bars"