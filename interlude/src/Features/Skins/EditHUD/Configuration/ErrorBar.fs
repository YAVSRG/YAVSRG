namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type ErrorBarPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let show_guide = Setting.simple config.TimingDisplayShowGuide
    let guide_thickness = Setting.percentf config.TimingDisplayGuideThickness
    let show_non_judgements = Setting.simple config.TimingDisplayShowNonJudgements
    let rotation = Setting.simple config.TimingDisplayRotation

    let thickness =
        config.TimingDisplayThickness |> Setting.bounded (1.0f, 25.0f)

    let release_thickness =
        config.TimingDisplayReleasesExtraHeight
        |> Setting.bounded (-20.0f, 20.0f)

    let windows_opacity = config.TimingDisplayWindowsOpacity |> Setting.bounded (0.0f, 0.6f)

    let half_scale_releases = Setting.simple config.TimingDisplayHalfScaleReleases

    let animation_time =
        config.TimingDisplayFadeTime
        |> Setting.bounded (100.0f<ms / rate>, 2000.0f<ms / rate>)

    let moving_average_type = Setting.simple config.TimingDisplayMovingAverageType
    let moving_average_sensitivity = config.TimingDisplayMovingAverageSensitivity |> Setting.bounded (0.01f, 1.0f)
    let moving_average_color = Setting.simple config.TimingDisplayMovingAverageColor

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.error_bar.rotation",
            SelectDropdown(
                [|
                    ErrorBarRotation.Normal, %"hud.error_bar.rotation.normal"
                    ErrorBarRotation.Clockwise, %"hud.error_bar.rotation.clockwise"
                    ErrorBarRotation.Anticlockwise, %"hud.error_bar.rotation.anticlockwise"
                |],
                rotation
            )
        )
            .Help(Help.Info("hud.error_bar.rotation"))
            .Pos(0)
        |+ PageSetting(%"hud.error_bar.shownonjudgements", Checkbox show_non_judgements)
            .Help(Help.Info("hud.error_bar.shownonjudgements"))
            .Pos(2)
        |+ PageSetting(%"hud.error_bar.halfscalereleases", Checkbox half_scale_releases)
            .Help(Help.Info("hud.error_bar.halfscalereleases"))
            .Pos(4)
        |+ PageSetting(%"hud.error_bar.thickness", Slider(thickness, Step = 1f))
            .Help(Help.Info("hud.error_bar.thickness"))
            .Pos(6)
        |+ PageSetting(%"hud.error_bar.showguide", Checkbox show_guide)
            .Help(Help.Info("hud.error_bar.showguide"))
            .Pos(8)
        |+ PageSetting(%"hud.error_bar.guide_thickness", Slider.Percent(guide_thickness))
            .Help(Help.Info("hud.error_bar.guide_thickness"))
            .Pos(10)
            .Conditional(show_guide.Get)
        |+ PageSetting(%"hud.error_bar.releasesextraheight", Slider(release_thickness, Step = 1f))
            .Help(Help.Info("hud.error_bar.releasesextraheight"))
            .Pos(12)
        |+ PageSetting(%"hud.error_bar.animationtime", Slider(Setting.uom animation_time, Step = 5f))
            .Help(Help.Info("hud.error_bar.animationtime"))
            .Pos(14)
        |+ PageSetting(%"hud.error_bar.timingwindowsopacity", Slider.Percent(windows_opacity))
            .Help(Help.Info("hud.error_bar.timingwindowsopacity"))
            .Pos(16)
        |+ PageSetting(%"hud.error_bar.moving_average_type",
            SelectDropdown(
                [|
                    ErrorBarMovingAverageType.None, %"hud.error_bar.moving_average_type.none"
                    ErrorBarMovingAverageType.Arrow, %"hud.error_bar.moving_average_type.arrow"
                    ErrorBarMovingAverageType.ReplaceBars, %"hud.error_bar.moving_average_type.replace_bars"
                |],
                moving_average_type
            )
        )
            .Help(Help.Info("hud.error_bar.moving_average_type"))
            .Pos(18)
        |+ PageSetting(%"hud.error_bar.moving_average_sensitivity", Slider.Percent(moving_average_sensitivity, Step = 0.01f))
            .Help(Help.Info("hud.error_bar.moving_average_sensitivity"))
            .Pos(20)
            .Conditional(fun () -> moving_average_type.Value <> ErrorBarMovingAverageType.None)
        |+ PageSetting(%"hud.error_bar.moving_average_color", ColorPicker(%"hud.error_bar.moving_average_color", moving_average_color, true))
            .Help(Help.Info("hud.error_bar.moving_average_color"))
            .Pos(22)
            .Conditional(fun () -> moving_average_type.Value <> ErrorBarMovingAverageType.None)
        :> Widget

    override this.Title = %"hud.error_bar"

    override this.OnClose() =
        Skins.save_hud_config
            { Content.HUD with
                TimingDisplayShowGuide = show_guide.Value
                TimingDisplayShowNonJudgements = show_non_judgements.Value
                TimingDisplayThickness = thickness.Value
                TimingDisplayGuideThickness = guide_thickness.Value
                TimingDisplayReleasesExtraHeight = release_thickness.Value
                TimingDisplayWindowsOpacity = windows_opacity.Value
                TimingDisplayHalfScaleReleases = half_scale_releases.Value
                TimingDisplayFadeTime = animation_time.Value
                TimingDisplayMovingAverageType = moving_average_type.Value
                TimingDisplayMovingAverageSensitivity = moving_average_sensitivity.Value
                TimingDisplayMovingAverageColor = moving_average_color.Value
                TimingDisplayRotation = rotation.Value
                TimingDisplayPosition =
                    if
                        (Content.HUD.TimingDisplayRotation <> ErrorBarRotation.Normal) <>
                        (rotation.Value <> ErrorBarRotation.Normal)
                    then
                        Content.HUD.TimingDisplayPosition.Rotate
                    else Content.HUD.TimingDisplayPosition
            }

        on_close ()