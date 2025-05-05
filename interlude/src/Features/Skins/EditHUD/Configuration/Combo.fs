namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play.HUD

type ComboPage() =
    inherit Page()

    let config = Content.HUD

    let lamp_colors = Setting.simple config.ComboLampColors

    let pop_amount =
        config.ComboPop
        |> Setting.bounded (0.0f, 20.0f)

    let growth_amount =
        config.ComboGrowth
        |> Setting.bounded (0.0f, 0.05f)

    let use_font = Setting.simple config.ComboUseFont
    let font_spacing =
        config.ComboFontSpacing
        |> Setting.bounded (-1.0f, 1.0f)

    let texture = Content.Texture "combo-font"
    let preview =
        { new ElementPreview(config.ComboPosition) with
            override this.DrawComponent(bounds) =
                if use_font.Value then
                    Combo.draw_combo_centered(texture, bounds, Color.White, 727, font_spacing.Value)
                else
                    Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)
        }

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                ComboLampColors = lamp_colors.Value
                ComboPop = pop_amount.Value
                ComboGrowth = growth_amount.Value
                ComboUseFont = use_font.Value
                ComboFontSpacing = font_spacing.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.combo.lampcolors", Checkbox lamp_colors)
                    .Help(Help.Info("hud.combo.lampcolors"))
                    .Pos(0),
                PageSetting(%"hud.combo.pop", Slider(pop_amount, Step = 1f))
                    .Help(Help.Info("hud.combo.pop"))
                    .Pos(2),
                PageSetting(%"hud.combo.growth", Slider(growth_amount))
                    .Help(Help.Info("hud.combo.growth"))
                    .Pos(4),
                PageSetting(%"hud.generic.use_font", Checkbox use_font)
                    .Help(Help.Info("hud.generic.use_font"))
                    .Pos(7),
                PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
                    .Help(Help.Info("hud.generic.font_spacing"))
                    .Pos(9)
                    .Conditional(use_font.Get)
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"hud.combo"