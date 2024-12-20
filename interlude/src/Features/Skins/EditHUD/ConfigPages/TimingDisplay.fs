﻿namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type TimingDisplayPage(on_close: unit -> unit) =
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
        |> Setting.bounded (0.0f, 20.0f)

    let windows_opacity = config.TimingDisplayWindowsOpacity |> Setting.bounded (0.0f, 0.6f)

    let half_scale_releases = Setting.simple config.TimingDisplayHalfScaleReleases

    let animation_time =
        config.TimingDisplayFadeTime
        |> Setting.bounded (100.0f<ms / rate>, 2000.0f<ms / rate>)

    let moving_average_type = Setting.simple config.TimingDisplayMovingAverageType
    let moving_average_sensitivity = config.TimingDisplayMovingAverageSensitivity |> Setting.bounded (0.01f, 0.5f)
    let moving_average_color = Setting.simple config.TimingDisplayMovingAverageColor

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.timingdisplay.rotation",
            SelectDropdown(
                [|
                    TimingDisplayRotation.Normal, %"hud.timingdisplay.rotation.normal"
                    TimingDisplayRotation.Clockwise, %"hud.timingdisplay.rotation.clockwise"
                    TimingDisplayRotation.Anticlockwise, %"hud.timingdisplay.rotation.anticlockwise"
                |],
                rotation
            )
        )
            .Help(Help.Info("hud.timingdisplay.rotation"))
            .Pos(0)
        |+ PageSetting(%"hud.timingdisplay.shownonjudgements", Checkbox show_non_judgements)
            .Help(Help.Info("hud.timingdisplay.shownonjudgements"))
            .Pos(2)
        |+ PageSetting(%"hud.timingdisplay.halfscalereleases", Checkbox half_scale_releases)
            .Help(Help.Info("hud.timingdisplay.halfscalereleases"))
            .Pos(4)
        |+ PageSetting(%"hud.timingdisplay.thickness", Slider(thickness, Step = 1f))
            .Help(Help.Info("hud.timingdisplay.thickness"))
            .Pos(6)
        |+ PageSetting(%"hud.timingdisplay.showguide", Checkbox show_guide)
            .Help(Help.Info("hud.timingdisplay.showguide"))
            .Pos(8)
        |+ PageSetting(%"hud.timingdisplay.guide_thickness", Slider.Percent(guide_thickness))
            .Help(Help.Info("hud.timingdisplay.guide_thickness"))
            .Pos(10)
            .Conditional(show_guide.Get)
        |+ PageSetting(%"hud.timingdisplay.releasesextraheight", Slider(release_thickness, Step = 1f))
            .Help(Help.Info("hud.timingdisplay.releasesextraheight"))
            .Pos(12)
        |+ PageSetting(%"hud.timingdisplay.animationtime", Slider(Setting.uom animation_time, Step = 5f))
            .Help(Help.Info("hud.timingdisplay.animationtime"))
            .Pos(14)
        |+ PageSetting(%"hud.timingdisplay.timingwindowsopacity", Slider.Percent(windows_opacity))
            .Help(Help.Info("hud.timingdisplay.timingwindowsopacity"))
            .Pos(16)
        |+ PageSetting(%"hud.timingdisplay.moving_average_type",
            SelectDropdown(
                [|
                    TimingDisplayMovingAverageType.None, %"hud.timingdisplay.moving_average_type.none"
                    TimingDisplayMovingAverageType.Arrow, %"hud.timingdisplay.moving_average_type.arrow"
                    TimingDisplayMovingAverageType.ReplaceBars, %"hud.timingdisplay.moving_average_type.replace_bars"
                |],
                moving_average_type
            )
        )
            .Help(Help.Info("hud.timingdisplay.moving_average_type"))
            .Pos(18)
        |+ PageSetting(%"hud.timingdisplay.moving_average_sensitivity", Slider.Percent(moving_average_sensitivity, Step = 0.01f))
            .Help(Help.Info("hud.timingdisplay.moving_average_sensitivity"))
            .Pos(20)
            .Conditional(fun () -> moving_average_type.Value <> TimingDisplayMovingAverageType.None)
        |+ PageSetting(%"hud.timingdisplay.moving_average_color", ColorPicker(moving_average_color, true))
            .Help(Help.Info("hud.timingdisplay.moving_average_color"))
            .Pos(22, 3)
            .Conditional(fun () -> moving_average_type.Value <> TimingDisplayMovingAverageType.None)
        :> Widget

    override this.Title = %"hud.timingdisplay"

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
                        (Content.HUD.TimingDisplayRotation <> TimingDisplayRotation.Normal) <>
                        (rotation.Value <> TimingDisplayRotation.Normal)
                    then
                        Content.HUD.TimingDisplayPosition.Rotate
                    else Content.HUD.TimingDisplayPosition
            }

        on_close ()