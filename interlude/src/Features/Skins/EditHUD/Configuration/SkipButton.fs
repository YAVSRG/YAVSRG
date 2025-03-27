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
    let background_scale = config.SkipButtonBackground.Scale |> Setting.bounded (0.5f, 2.0f)
    let background_offset_x = Setting.percentf config.SkipButtonBackground.AlignmentX
    let background_offset_y = Setting.percentf config.SkipButtonBackground.AlignmentY

    let preview_text = [ (%%"skip").ToString() ] %> "play.skiphint"

    let preview =
        { new ElementPreview(config.SkipButtonPosition) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, preview_text, bounds, Colors.text, Alignment.CENTER)
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.skip_button.usebackground", Checkbox use_background)
            .Help(Help.Info("hud.skip_button.usebackground"))
            .Pos(0)
        |+ PageSetting(%"hud.skip_button.backgroundscale", Slider.Percent(background_scale))
            .Help(Help.Info("hud.skip_button.backgroundscale"))
            .Pos(2)
            .Conditional(use_background.Get)
        |+ PageSetting(%"hud.skip_button.background_offset_x", Slider.Percent(background_offset_x))
            .Help(Help.Info("hud.skip_button.background_offset_x"))
            .Pos(4)
            .Conditional(use_background.Get)
        |+ PageSetting(%"hud.skip_button.background_offset_y", Slider.Percent(background_offset_y))
            .Help(Help.Info("hud.skip_button.background_offset_y"))
            .Pos(6)
            .Conditional(use_background.Get)
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"hud.skip_button"

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