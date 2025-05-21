namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type HudGraphPage(on_close: unit -> unit) = 
    inherit Page()

    let config = Content.HUD
    let hit_size = config.HudGraphHitSize |> Setting.bounded (1.0f, 15.0f)
    let scroll_speed = config.HudGraphScrollSpeed |> Setting.bounded (0.0f, 100.0f)
    let window_opacity = config.HudGraphWindowOpacity |> Setting.bounded (0.0f, 100.0f)
    let hit_opacity = config.HudGraphHitOpacity |> Setting.bounded (0.0f, 100.0f)
    let draw_border = Setting.simple config.HudGraphDrawBorder 
    let transparent = Setting.simple config.HudGraphTransparent
    let show_incoming_notes = Setting.simple config.HudGraphShowIncomingNotes

    override this.Content() =
        page_container()
         |+ PageSetting(%"hud.hudgraph.hitsize", Slider (hit_size, Step = 1f))
            .Help(Help.Info("hud.hudgraph.hitsize"))
            .Pos(0)
         |+ PageSetting(%"hud.hudgraph.scrollspeed", Slider (scroll_speed, Step = 0.1f))
            .Help(Help.Info("hud.hudgraph.scrollspeed"))
            .Pos(2)
         |+ PageSetting(%"hud.hudgraph.windowopacity", Slider (window_opacity, Step = 1f))
            .Help(Help.Info("hud.hudgraph.windowopacity"))
            .Pos(4)
         |+ PageSetting(%"hud.hudgraph.hitopacity", Slider (hit_opacity, Step = 1f))
            .Help(Help.Info("hud.hudgraph.hitopacity"))
            .Pos(6)
         |+ PageSetting(%"hud.hudgraph.drawborder", Checkbox draw_border )
            .Help(Help.Info("hud.hudgraph.drawborder"))
            .Pos(9)
         |+ PageSetting(%"hud.hudgraph.transparent", Checkbox transparent )
            .Help(Help.Info("hud.hudgraph.transparent"))
            .Pos(11)
         |+ PageSetting(%"hud.hudgraph.showincomingnotes", Checkbox show_incoming_notes )
            .Help(Help.Info("hud.hudgraph.showincomingnotes"))
            .Pos(13)
        :> Widget

    override this.Title = %"hud.hudgraph"
    override this.OnClose() =
        Skins.save_hud_config
            { Content.HUD with
                HudGraphHitSize = hit_size.Value
                HudGraphScrollSpeed = scroll_speed.Value
                HudGraphWindowOpacity = window_opacity.Value
                HudGraphHitOpacity = hit_opacity.Value
                HudGraphDrawBorder = draw_border.Value
                HudGraphTransparent = transparent.Value
                HudGraphShowIncomingNotes = show_incoming_notes.Value
            }

        on_close ()

