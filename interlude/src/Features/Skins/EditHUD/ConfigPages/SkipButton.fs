namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type SkipButtonPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let use_background = Setting.simple config.SkipButtonBackground.Enable
    let background_scale = Setting.simple config.SkipButtonBackground.Scale |> Setting.bound 0.5f 2.0f
    let background_offset_x = Setting.percentf config.SkipButtonBackground.AlignmentX
    let background_offset_y = Setting.percentf config.SkipButtonBackground.AlignmentY

    let preview_text = [ (%%"skip").ToString() ] %> "play.skiphint"

    let preview =
        { new ConfigPreviewNew(config.SkipButtonPosition) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, preview_text, bounds, Colors.text, Alignment.CENTER)
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.skipbutton.usebackground", Checkbox use_background)
            .Help(Help.Info("hud.skipbutton.usebackground"))
            .Pos(0)
        |+ PageSetting(%"hud.skipbutton.backgroundscale", Slider.Percent(background_scale))
            .Help(Help.Info("hud.skipbutton.backgroundscale"))
            .Pos(2)
            .Conditional(use_background.Get)
        |+ PageSetting(%"hud.skipbutton.background_offset_x", Slider.Percent(background_offset_x))
            .Help(Help.Info("hud.skipbutton.background_offset_x"))
            .Pos(4)
            .Conditional(use_background.Get)
        |+ PageSetting(%"hud.skipbutton.background_offset_y", Slider.Percent(background_offset_y))
            .Help(Help.Info("hud.skipbutton.background_offset_y"))
            .Pos(6)
            .Conditional(use_background.Get)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.skipbutton"

    override this.OnClose() =
        Skins.save_hud_config 
            { Content.HUD with
                SkipButtonBackground = 
                    {
                        Enable = use_background.Value
                        Scale = background_scale.Value
                        AlignmentX = background_offset_x.Value
                        AlignmentY = background_offset_y.Value
                    }
            }
        on_close ()