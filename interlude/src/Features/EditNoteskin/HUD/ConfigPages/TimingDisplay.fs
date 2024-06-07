namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type TimingDisplayPage(on_close: unit -> unit) =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.TimingDisplayPosition

    let show_guide = Setting.simple user_options.TimingDisplayShowGuide
    let show_non_judgements = Setting.simple user_options.TimingDisplayShowNonJudgements

    let thickness =
        Setting.simple user_options.TimingDisplayThickness |> Setting.bound 1.0f 25.0f

    let release_thickness =
        Setting.simple user_options.TimingDisplayReleasesExtraHeight
        |> Setting.bound 0.0f 20.0f

    let half_scale_releases = Setting.simple user_options.TimingDisplayHalfScaleReleases

    let animation_time =
        Setting.simple user_options.TimingDisplayFadeTime
        |> Setting.bound 100.0f 2000.0f

    let moving_average_type = Setting.simple user_options.TimingDisplayMovingAverageType
    let moving_average_sensitivity = Setting.simple user_options.TimingDisplayMovingAverageSensitivity |> Setting.bound 0.01f 0.5f
    let moving_average_color = Setting.simple noteskin_options.TimingDisplayMovingAverageColor

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect
                    (Rect.Create(
                        bounds.CenterX - thickness.Value / 2.0f,
                        bounds.Top,
                        bounds.CenterX + thickness.Value / 2.0f,
                        bounds.Bottom
                    ))
                    Color.White
        }

    override this.Content() = 
        page_container()
        |+ PageSetting(%"hud.timingdisplay.showguide", Checkbox show_guide)
            .Tooltip(Tooltip.Info("hud.timingdisplay.showguide"))
            .Pos(0)
        |+ PageSetting(%"hud.timingdisplay.shownonjudgements", Checkbox show_non_judgements)
            .Tooltip(Tooltip.Info("hud.timingdisplay.shownonjudgements"))
            .Pos(2)
        |+ PageSetting(%"hud.timingdisplay.halfscalereleases", Checkbox half_scale_releases)
            .Tooltip(Tooltip.Info("hud.timingdisplay.halfscalereleases"))
            .Pos(4)
        |+ PageSetting(%"hud.timingdisplay.thickness", Slider(thickness, Step = 1f))
            .Tooltip(Tooltip.Info("hud.timingdisplay.thickness"))
            .Pos(6)
        |+ PageSetting(%"hud.timingdisplay.releasesextraheight", Slider(release_thickness, Step = 1f))
            .Tooltip(Tooltip.Info("hud.timingdisplay.releasesextraheight"))
            .Pos(8)
        |+ PageSetting(%"hud.timingdisplay.animationtime", Slider(animation_time, Step = 5f))
            .Tooltip(Tooltip.Info("hud.timingdisplay.animationtime"))
            .Pos(10)
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
            .Tooltip(Tooltip.Info("hud.timingdisplay.moving_average_type"))
            .Pos(12)
        |+ PageSetting(%"hud.timingdisplay.moving_average_sensitivity", Slider.Percent(moving_average_sensitivity, Step = 0.01f))
            .Tooltip(Tooltip.Info("hud.timingdisplay.moving_average_sensitivity"))
            .Pos(14)
            .Conditional(fun () -> moving_average_type.Value <> TimingDisplayMovingAverageType.None)
        |+ ([
            PageSetting(%"hud.timingdisplay.moving_average_color", ColorPicker(moving_average_color, true))
                .Tooltip(Tooltip.Info("hud.timingdisplay.moving_average_color"))
                .Pos(16, 3)
                .Conditional(fun () -> moving_average_type.Value <> TimingDisplayMovingAverageType.None)
            :> Widget
        ] |> or_require_noteskin)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.timingdisplay"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                TimingDisplayShowGuide = show_guide.Value
                TimingDisplayShowNonJudgements = show_non_judgements.Value
                TimingDisplayThickness = thickness.Value
                TimingDisplayReleasesExtraHeight = release_thickness.Value
                TimingDisplayFadeTime = animation_time.Value
                TimingDisplayMovingAverageType = moving_average_type.Value
                TimingDisplayMovingAverageSensitivity = moving_average_sensitivity.Value
            }
        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                TimingDisplayMovingAverageColor = moving_average_color.Value
            }

        on_close ()