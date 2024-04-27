namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type EarlyLateMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.EarlyLateMeterPosition

    let duration =
        Setting.simple noteskin_options.EarlyLateMeterDuration
        |> Setting.bound 100.0f 2000.0f

    let frame_time =
        Setting.simple noteskin_options.EarlyLateMeterFrameTime
        |> Setting.bound 2.0f 500.0f

    let use_texture = Setting.simple noteskin_options.EarlyLateMeterUseTexture

    let early_text = Setting.simple noteskin_options.EarlyLateMeterEarlyText
    let late_text = Setting.simple noteskin_options.EarlyLateMeterLateText
    let early_color = Setting.simple noteskin_options.EarlyLateMeterEarlyColor
    let late_color = Setting.simple noteskin_options.EarlyLateMeterLateColor

    do
        this.Content(
            page_container()
            |+ ([
                PageSetting("hud.earlylatemeter.duration", Slider(duration, Step = 5f))
                    .Pos(0)
                    .Tooltip(Tooltip.Info("hud.earlylatemeter.duration")) :> Widget
                PageSetting("hud.earlylatemeter.usetexture", Selector<_>.FromBool(use_texture))
                    .Pos(2)
                    .Tooltip(Tooltip.Info("hud.earlylatemeter.usetexture"))
                Conditional(use_texture.Get, 
                    PageSetting("hud.earlylatemeter.frametime", Slider(frame_time, Step = 5f))
                        .Pos(4)
                        .Tooltip(Tooltip.Info("hud.earlylatemeter.frametime"))
                )
                Conditional(use_texture.Get >> not, 
                    PageTextEntry("hud.earlylatemeter.earlytext", early_text)
                        .Pos(4)
                        .Tooltip(Tooltip.Info("hud.earlylatemeter.earlytext"))
                )
                Conditional(use_texture.Get >> not, 
                    PageSetting("hud.earlylatemeter.earlycolor", ColorPicker(early_color, false))
                        .Pos(6, 3)
                        .Tooltip(Tooltip.Info("hud.earlylatemeter.earlycolor"))
                )
                Conditional(use_texture.Get >> not, 
                    PageTextEntry("hud.earlylatemeter.latetext", late_text)
                        .Pos(9)
                        .Tooltip(Tooltip.Info("hud.earlylatemeter.latetext"))
                )
                Conditional(use_texture.Get >> not, 
                    PageSetting("hud.earlylatemeter.latecolor", ColorPicker(late_color, false))
                        .Pos(11, 3)
                        .Tooltip(Tooltip.Info("hud.earlylatemeter.latecolor"))
                )
                ] |> or_require_noteskin)
            |>> Container
        )

    override this.Title = %"hud.earlylatemeter.name"

    override this.OnClose() =
        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                EarlyLateMeterPosition = pos.Value
                EarlyLateMeterDuration = duration.Value
                EarlyLateMeterUseTexture = use_texture.Value
                EarlyLateMeterEarlyText = early_text.Value
                EarlyLateMeterEarlyColor = early_color.Value
                EarlyLateMeterLateText = late_text.Value
                EarlyLateMeterLateColor = late_color.Value
            }

        on_close ()