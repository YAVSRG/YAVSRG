namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type HudGraphPage() = 
    inherit Page()

    let config = Content.HUD

    let hit_size = 
        config.HudGraphHitSize 
        |> Setting.bounded (1.0f, 25.0f)

    let window_opacity = 
        config.HudGraphWindowOpacity 
        |> Setting.bounded (0.0f, 100.0f)

    let hit_opacity = 
        config.HudGraphHitOpacity 
        |> Setting.bounded (0.0f, 100.0f)

    let smooth_scrolling = Setting.simple config.HudGraphSmoothScrolling

    let draw_border = Setting.simple config.HudGraphDrawBorder 

    let transparent = Setting.simple config.HudGraphTransparent

    let show_incoming_notes = Setting.simple config.HudGraphShowIncomingNotes

    let scroll_type = Setting.simple config.HudGraphScrollType

    let scroll_speed = 
        config.HudGraphScrollSpeed 
        |> Setting.bounded (0.0f, 25.0f)

    let show_x_notes = 
        config.HudGraphShowXNotes
        |> Setting.bounded (1f, 250f)

    let show_whole_chart = Setting.simple config.HudGraphShowWholeChart 


    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                HudGraphHitSize = hit_size.Value
                HudGraphScrollSpeed = scroll_speed.Value
                HudGraphWindowOpacity = window_opacity.Value
                HudGraphHitOpacity = hit_opacity.Value
                HudGraphShowXNotes = show_x_notes.Value
                HudGraphShowWholeChart = show_whole_chart.Value
                HudGraphDrawBorder = draw_border.Value
                HudGraphTransparent = transparent.Value
                HudGraphShowIncomingNotes = show_incoming_notes.Value
                HudGraphSmoothScrolling = smooth_scrolling.Value
                HudGraphScrollType = scroll_type.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.hud_graph.hit_size", Slider (hit_size, Step = 1f))
                    .Help(Help.Info("hud.hud_graph.hit_size"))
                    .Pos(0),
                PageSetting(%"hud.hud_graph.window_opacity", Slider (window_opacity, Step = 1f))
                    .Help(Help.Info("hud.hud_graph.window_opacity"))
                    .Pos(2),
                PageSetting(%"hud.hud_graph.hit_opacity", Slider (hit_opacity, Step = 1f))
                    .Help(Help.Info("hud.hud_graph.hit_opacity"))
                    .Pos(4),
                PageSetting(%"hud.hud_graph.draw_border", Checkbox draw_border)
                    .Help(Help.Info("hud.hud_graph.draw_border"))
                    .Pos(7),
                PageSetting(%"hud.hud_graph.transparent", Checkbox transparent)
                    .Help(Help.Info("hud.hud_graph.transparent"))
                    .Pos(9),
                PageSetting(%"hud.hud_graph.show_incoming_notes", Checkbox show_incoming_notes)
                    .Help(Help.Info("hud.hud_graph.show_incoming_notes"))
                    .Pos(11),
                PageSetting(%"hud.hud_graph.smooth_scrolling", Checkbox smooth_scrolling)
                    .Help(Help.Info("hud.hud_graph.smooth_scrolling"))
                    .Pos(13),
                PageSetting(%"hud.hud_graph.scroll_type",
                    SelectDropdown(
                        [|
                            HudGraphScrollType.ScrollSpeed, %"hud.hud_graph.scrolltype.scroll_speed"
                            HudGraphScrollType.ShowWholeChart, %"hud.hud_graph.scrolltype.show_whole_chart"
                            HudGraphScrollType.ShowXNotes, %"hud.hud_graph.scrolltype.show_x_notes"
                        |],
                        scroll_type
                    )
                )
                    .Help(Help.Info("hud.hud_graph.scroll_type"))
                    .Pos(16),
                PageSetting(%"hud.hud_graph.scroll_speed", Slider (scroll_speed, Step = 0.1f))
                    .Help(Help.Info("hud.hud_graph.scroll_speed"))
                    .Pos(18)
                    .Conditional (fun () -> scroll_type.Value = HudGraphScrollType.ScrollSpeed),
                PageSetting(%"hud.hud_graph.show_x_notes", Slider (show_x_notes, Step = 1f))
                    .Help(Help.Info("hud.hud_graph.show_x_notes"))
                    .Pos(18)
                    .Conditional (fun () -> scroll_type.Value = HudGraphScrollType.ShowXNotes)


            )

    override this.Title = %"hud.hud_graph"

