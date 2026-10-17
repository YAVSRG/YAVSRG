namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type ErrorBarWindowPage() =
    inherit Page()

    let config = Content.HUD

    let windows_opacity = config.TimingDisplayWindowsOpacity |> Setting.bounded (0.0f, 0.6f)

    let log_scale_sensitivity = config.TimingDisplayLogSensitivity |> Setting.bounded (0.0f, 1.0f)

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                TimingDisplayWindowsOpacity = windows_opacity.Value
                TimingDisplayLogSensitivity = log_scale_sensitivity.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.error_bar.timingwindowsopacity", Slider.Percent(windows_opacity))
                    .Help(Help.Info("hud.error_bar.timingwindowsopacity"))
                    .Pos(0),
                PageSetting(%"hud.error_bar.logsensitivity", Slider.Percent(log_scale_sensitivity, Step = 0.01f))
                    .Help(Help.Info("hud.error_bar.logsensitivity"))
                    .Pos(2)
            )
    

    override this.Title = %"hud.error_bar.timing_windows_settings"