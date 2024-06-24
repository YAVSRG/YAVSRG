namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI.Menu

type EarlyLateMeterPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let duration =
        Setting.simple config.EarlyLateMeterDuration
        |> Setting.bound 100.0f 2000.0f

    let frame_time =
        Setting.simple config.EarlyLateMeterFrameTime
        |> Setting.bound 2.0f 500.0f

    let use_texture = Setting.simple config.EarlyLateMeterUseTexture

    let early_text = Setting.simple config.EarlyLateMeterEarlyText
    let late_text = Setting.simple config.EarlyLateMeterLateText
    let early_color = Setting.simple config.EarlyLateMeterEarlyColor
    let late_color = Setting.simple config.EarlyLateMeterLateColor

    override this.Content() =
        page_container()
        |+ ([
            PageSetting(%"hud.earlylatemeter.duration", Slider(duration, Step = 5f))
                .Tooltip(Tooltip.Info("hud.earlylatemeter.duration"))
                .Pos(0)
            PageSetting(%"hud.earlylatemeter.usetexture", Checkbox use_texture)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.usetexture"))
                .Pos(2)
            PageSetting(%"hud.earlylatemeter.frametime", Slider(frame_time, Step = 5f))
                .Tooltip(Tooltip.Info("hud.earlylatemeter.frametime"))
                .Pos(4)
                .Conditional(use_texture.Get)
            PageTextEntry(%"hud.earlylatemeter.earlytext", early_text)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.earlytext"))
                .Pos(4)
                .Conditional(use_texture.Get >> not)
            PageSetting(%"hud.earlylatemeter.earlycolor", ColorPicker(early_color, false))
                .Tooltip(Tooltip.Info("hud.earlylatemeter.earlycolor"))
                .Pos(6, 3)
                .Conditional(use_texture.Get >> not)
            PageTextEntry(%"hud.earlylatemeter.latetext", late_text)
                .Tooltip(Tooltip.Info("hud.earlylatemeter.latetext"))
                .Pos(9)
                .Conditional(use_texture.Get >> not)
            PageSetting(%"hud.earlylatemeter.latecolor", ColorPicker(late_color, false))
                .Tooltip(Tooltip.Info("hud.earlylatemeter.latecolor"))
                .Pos(11, 3)
                .Conditional(use_texture.Get >> not)
            ] |> or_require_noteskin)
        :> Widget

    override this.Title = %"hud.earlylatemeter"

    override this.OnClose() =
        HUDs.save_config 
            { Content.HUD with
                EarlyLateMeterDuration = duration.Value
                EarlyLateMeterUseTexture = use_texture.Value
                EarlyLateMeterEarlyText = early_text.Value
                EarlyLateMeterEarlyColor = early_color.Value
                EarlyLateMeterLateText = late_text.Value
                EarlyLateMeterLateColor = late_color.Value
            }

        on_close ()