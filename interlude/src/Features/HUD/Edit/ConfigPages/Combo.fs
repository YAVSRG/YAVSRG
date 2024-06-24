namespace Interlude.Features.HUD.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Features.Play.HUD

type ComboPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let lamp_colors = Setting.simple config.ComboLampColors

    let pop_amount =
        Setting.simple config.ComboPop |> Setting.bound 0.0f 20.0f

    let growth_amount =
        Setting.simple config.ComboGrowth |> Setting.bound 0.0f 0.05f

    let use_font = Setting.simple config.ComboUseFont
    let font_spacing = Setting.simple config.ComboFontSpacing |> Setting.bound -1.0f 1.0f

    let texture = Content.Texture "combo-font"
    let preview =
        { new ConfigPreviewNew(config.ComboPosition) with
            override this.DrawComponent(bounds) =
                if use_font.Value then
                    Combo.draw_combo_centered(texture, bounds, Color.White, 727, font_spacing.Value)
                else
                    Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.combo.lampcolors", Checkbox lamp_colors)
            .Tooltip(Tooltip.Info("hud.combo.lampcolors"))
            .Pos(0)
        |+ PageSetting(%"hud.combo.pop", Slider(pop_amount, Step = 1f))
            .Tooltip(Tooltip.Info("hud.combo.pop"))
            .Pos(2)
        |+ PageSetting(%"hud.combo.growth", Slider(growth_amount))
            .Tooltip(Tooltip.Info("hud.combo.growth"))
            .Pos(4)
        |+ PageSetting(%"hud.generic.use_font", Checkbox use_font)
            .Tooltip(Tooltip.Info("hud.generic.use_font"))
            .Pos(7)
        |+ PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
            .Tooltip(Tooltip.Info("hud.generic.font_spacing"))
            .Pos(9)
            .Conditional(use_font.Get)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.combo"

    override this.OnClose() =
        HUD.save_config 
            { Content.HUD with
                ComboLampColors = lamp_colors.Value
                ComboPop = pop_amount.Value
                ComboGrowth = growth_amount.Value
                ComboUseFont = use_font.Value
                ComboFontSpacing = font_spacing.Value
            }

        on_close ()